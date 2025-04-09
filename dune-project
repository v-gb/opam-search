(lang dune 3.15)

(formatting
 (enabled_for ocaml))

(name opam-search)
(generate_opam_files true)
(source (github v-gb/opam-search))
(authors "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(maintainers "Valentin Gatien-Baron <valentin.gatienbaron@gmail.com>")
(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")
(documentation https://github.com/v-gb/ocamlmig/blob/main/README.md)

(package
 (name opam-search)
 (synopsis "Run code on the opam source of specified packages")
 (description "Run code on the opam source of specified packages")
 (depends
   ocaml
   dune
   ;;
   base
   core
   core_unix
   ppx_partial
 ))
