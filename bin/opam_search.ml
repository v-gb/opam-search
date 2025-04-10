module ZUnix = Unix
open Core
module Unix = ZUnix

module Process_res = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Detailed : ('a, ('a, Unix.process_status * Core.Sexp.t) Result.t) t
end

let with_process_full ?(env = []) ?cwd (type a res) (process_res : (a, res) Process_res.t)
    argv f : res =
  let argv =
    match cwd with
    | None -> argv
    | Some cwd ->
        (* We should probably use spawn in general. *)
        [ "bash"
        ; "-c"
        ; (let env =
             List.map env ~f:(fun (k, v_opt) ->
                 match v_opt with
                 | None -> "unset " ^ Core.Sys.quote k ^ " && "
                 | Some v ->
                     "export " ^ Core.Sys.quote k ^ "=" ^ Core.Sys.quote v ^ " && ")
             |> String.concat
           in
           env ^ "cd " ^ Core.Sys.quote cwd ^ " && " ^ Core.Sys.concat_quoted argv)
        ]
  in
  let prog = Option.value (List.hd argv) ~default:"" in
  let res = ref (Unix.WEXITED 0) in
  let (fres : a), stderr =
    Exn.protectx
      ~finally:(fun channels -> res := Unix.close_process_full channels)
      (Unix.open_process_args_full prog (Array.of_list argv) (Unix.environment ()))
      ~f:(fun (stdout, stdin, stderr) ->
        let res = f (stdout, stdin) in
        Out_channel.close stdin;
        In_channel.close stdout;
        (res, In_channel.input_all stderr))
  in
  match !res with
  | WEXITED 0 -> ( match process_res with Raise -> fres | Detailed -> Ok fres)
  | _ -> (
      let explain =
        match !res with
        | WEXITED n -> "code " ^ Int.to_string n
        | WSIGNALED n -> "signal " ^ Int.to_string n
        | WSTOPPED _ -> assert false
      in
      let sexp =
        [%sexp
          ("process exited with " ^ explain : string)
        , (argv : string list)
        , ~~(stderr : string)]
      in
      match process_res with Raise -> raise_s sexp | Detailed -> Error (!res, sexp))

let run_process ?env ?cwd process_res argv =
  with_process_full ?env ?cwd process_res argv (fun (stdout, stdin) ->
      Out_channel.close stdin;
      (* we should use eio or async, as this can deadlock if stderr is big enough *)
      In_channel.input_all stdout)

let read_file_or_cache fname f =
  if not (Sys_unix.file_exists_exn fname) then f fname;
  In_channel.read_all fname

let concurrently a f =
  (* Two stages in the computation we provide: the first stage is parallel, the second
     stage is sequential. This allows to do things like [parallel -k], where the output
     is printed in input order, depending on which stage the print happens in. *)
  let chan = Domainslib.Chan.make_unbounded () in
  let ndoms = Domain.recommended_domain_count () - 1 in
  for i_dom = 0 to ndoms - 1 do
    let rec self =
      lazy
        (Domain.spawn (fun () ->
             let chunk_size = (Array.length a + ndoms - 1) / ndoms in
             let start = chunk_size * i_dom in
             let end_ = min (start + chunk_size) (Array.length a) in
             for i = start to end_ - 1 do
               Domainslib.Chan.send chan (`Elt (i, f a.(i)))
             done;
             Domainslib.Chan.send chan (`Join (fun () -> force self))))
    in
    ignore (force self)
  done;
  let rec drain next buf ndoms =
    match Map.find buf next with
    | Some (lazy ()) -> drain (next + 1) (Map.remove buf next) ndoms
    | None -> (
        if ndoms = 0
        then ()
        else
          match Domainslib.Chan.recv chan with
          | `Join domain ->
              Domain.join (domain ());
              drain next buf (ndoms - 1)
          | `Elt (i, seq) -> drain next (Map.add_exn buf ~key:i ~data:seq) ndoms)
  in
  drain 0 (Map.empty (module Int)) ndoms

let with_tmpdir f =
  let tmp_fname = Filename_unix.temp_dir "opam-search" "" in
  Exn.protect
    ~finally:(fun () ->
      if false
      then print_s [%sexp "left", (tmp_fname : string)]
      else Sys_unix.command_exn ("rm -rf -- " ^ Sys.quote tmp_fname))
    ~f:(fun () -> f tmp_fname)

let curl ~dst_is_cache ~dst url =
  if dst_is_cache && Sys_unix.file_exists_exn dst
  then true
  else (
    Sys_unix.command_exn ("mkdir -p -- " ^ Sys.quote (Filename.dirname dst));
    let s =
      run_process Raise
        [ "curl"
        ; "--write-out"
        ; "%{http_code}"
        ; "--retry"
        ; "3"
        ; "--retry-delay"
        ; "2"
        ; "--user-agent"
        ; "opam-search"
        ; "-L"
        ; "-o"
        ; dst
        ; "--"
        ; url
        ]
    in
    match Int.of_string s with
    | 404 -> false
    | 200 -> true
    | _ -> failwith ("failed with code " ^ s))

let debug = false

let curl_untar ~cache url ~dst_dir =
  let time_before = Time_ns.now () in
  let dst = match cache with None -> dst_dir ^/ "dl.tar.gz" | Some v -> v in
  curl ~dst_is_cache:(Option.is_some cache) ~dst url
  && (match
        run_process Detailed ~cwd:dst_dir [ "tar"; "--strip-component=1"; "-xf"; dst ]
      with
     | Ok _ -> true
     | Error (_, s) ->
         (* Apparently this happens when we download a .zip instead of .tar.gz. I
           suppose we could check the extension of what we're getting, or the magic
           bits, but meh, it's like 20 packages out of 3200. *)
         String.is_substring (Sexp.to_string s)
           ~substring:"gzip: stdin has more than one entry")
  &&
  let duration = Time_ns.diff (Time_ns.now ()) time_before in
  if debug then print_s [%sexp "curl untar", (url : string), (duration : Time_ns.Span.t)];
  true

let run ~packages ~src f =
  let xdg = Xdg.create ~env:Sys.getenv () in
  let packages =
    match packages with
    | _ :: _ -> packages
    | [] ->
        read_file_or_cache "/tmp/opam-cache-packages" (fun f ->
            Sys_unix.command_exn
              ("opam list -a --short --coinstallable-with=ocaml.5.2 | LANG=C sort > "
              ^ Sys.quote f))
        |> String.split_lines
  in
  (* let packages = List.take packages 500 in *)
  let digest = Md5.to_hex (Md5.digest_string (String.concat_lines packages)) in
  let package_infos =
    read_file_or_cache ("/tmp/opam-cache-show-" ^ digest) (fun f ->
        Sys_unix.command_exn
          ("opam show "
          ^ Sys.concat_quoted packages
          ^ " -f name,dev-repo,url.checksum --raw > "
          ^ Sys.quote f))
    |> String.split_lines
    |> fun l ->
    let name = ref None in
    let key = ref None in
    let current_checksum = ref None in
    let dev_repo = ref None in
    let q = Queue.create () in
    let flush () =
      (match (!name, !current_checksum, !dev_repo) with
      | Some name, Some md5, Some dev_repo -> Queue.enqueue q (name, md5, dev_repo)
      | _ -> ());
      name := None;
      current_checksum := None;
      dev_repo := None
    in
    List.iter l ~f:(fun line ->
        let key, value =
          match String.lsplit2 line ~on:':' with
          | None -> (Option.value_exn !key, line)
          | Some (l, r) ->
              key := Some l;
              (l, r)
        in
        let key = String.strip key in
        let value =
          String.strip ~drop:(function ' ' | '"' -> true | _ -> false) value
        in
        if String.( = ) key "name"
        then (
          flush ();
          name := Some value)
        else if String.( = ) key "dev-repo"
        then dev_repo := Some value
        else if String.( = ) key "url.checksum"
        then
          match String.chop_prefix value ~prefix:"md5=" with
          | None -> ()
          | Some md5 -> current_checksum := Some md5);
    flush ();
    Queue.to_list q
  in
  if debug then print_s [%sexp (package_infos : (string * string * string) list)];
  match src with
  | `Opam_cache ->
      let package_infos =
        let seen = Hash_set.create (module String) in
        package_infos
        |> List.filter_map ~f:(fun (name, md5, _) ->
               match Hash_set.strict_add seen md5 with
               | Error _ -> None
               | Ok () -> Some (md5, name))
        |> Array.of_list
      in
      let string_of_exn name e =
        String.split_lines (Exn.to_string e)
        |> List.map ~f:(Printf.sprintf "%s:%s" name)
        |> String.concat_lines
      in
      let catching_exn name f =
        match f () with
        | exception e -> lazy (print_string (string_of_exn name e))
        | lazy_ -> lazy (try force lazy_ with e -> print_string (string_of_exn name e))
      in
      concurrently package_infos (fun (md5, name) ->
          catching_exn name (fun () ->
              with_tmpdir (fun tmpdir ->
                  let hash_suffix = String.prefix md5 2 ^ "/" ^ md5 in
                  let url = "https://opam.ocaml.org/cache/md5/" ^ hash_suffix in
                  let cache = Xdg.cache_dir xdg ^/ "opam-search" ^/ hash_suffix in
                  if curl_untar ~cache:(Some cache) url ~dst_dir:tmpdir
                  then f ~name tmpdir
                  else lazy ())))
  | `Github ->
      let name_dev_repo =
        List.map package_infos ~f:(fun (name, _md5, url) -> (url, name))
        |> Hashtbl.of_alist_multi (module String)
      in
      Hashtbl.iteri name_dev_repo ~f:(fun ~key:url ~data:_repos ->
          match
            String.chop_prefix url ~prefix:"git+https://github.com/"
            |> Option.bind ~f:(String.chop_suffix ~suffix:".git")
          with
          | None -> if debug then print_s [%sexp "skip url", (url : string)]
          | Some url_rest ->
              with_tmpdir (fun tmpdir ->
                  if
                    List.exists [ "main"; "master" ] ~f:(fun branch ->
                        let url =
                          "https://github.com/"
                          ^ url_rest
                          ^ "/archive/refs/heads/"
                          ^ branch
                          ^ ".tar.gz"
                        in
                        curl_untar ~cache:None url ~dst_dir:tmpdir)
                  then force (f ~name:url tmpdir)
                  else if debug
                  then print_s [%sexp "failed to dl", (url : string)]))

let flag_optional_with_default_doc_custom name ~all:all_v ~to_string ~default ~doc =
  let open Command.Let_syntax.Let_syntax.Open_on_rhs in
  flag name
    (optional_with_default default
       (Arg_type.of_alist_exn ~list_values_in_help:false
          (List.map all_v ~f:(fun v -> (to_string v, v)))))
    ~doc:
      (Printf.sprintf "%s %s (default: %s)"
         (String.concat ~sep:"|" (List.map ~f:to_string all_v))
         doc (to_string default))

let cmd =
  Command.basic ~summary:"Retrieve specified opam packages, and run code on them"
    [%map_open.Command
      let src =
        flag_optional_with_default_doc_custom "src" ~all:[ `Opam_cache; `Github ]
          ~to_string:(function `Opam_cache -> "opam-cache" | `Github -> "github")
          ~default:`Opam_cache ~doc:"where to retrieve the source files from"
      and packages =
        flag "-p" (listed string)
          ~doc:
            "PACKAGE which opam packages to run on. If not specified, run on all \
             packages compatible with ocaml 5.2."
      and argv =
        flag "--" escape
          ~doc:
            "ARGS The command to run on all source files, for instance grep -R foo. The \
             working directory of the command will be the root of the package."
      and exit_codes =
        flag "-x" (listed int)
          ~doc:
            "EXIT_CODE exit codes that will be ignored. In particular, -x 1 is useful \
             when ARGS is a call to grep, to treat no matches as a success"
      and unprefixed =
        flag "-unprefixed" no_arg
          ~doc:
            "by default, the name of the package is added in front of lines of output. \
             This option disables the behavior. Instead, the command can consult the \
             PACKAGE env var to clarify what package the output is from."
      in
      fun () ->
        match argv with
        | None | Some [] ->
            failwith
              "the command to execute is required. opam-search -- rg -g '*.ml', for \
               instance"
        | Some argv ->
            run ~packages ~src (fun ~name dir ->
                let first_stage =
                  run_process ~env:[ ("PACKAGE", Some name) ] Detailed ~cwd:dir argv
                in
                lazy
                  (match first_stage with
                  | Ok s ->
                      if unprefixed
                      then print_string s
                      else
                        print_string
                          (String.split_lines s
                          |> List.map ~f:(Printf.sprintf "%s:%s" name)
                          |> String.concat_lines)
                  | Error (WEXITED n, _) when List.mem exit_codes n ~equal:( = ) -> ()
                  | Error (_, sexp) -> raise_s sexp))]

let main () = Command_unix.run ~version:"%%VERSION%%" cmd
let () = main ()
