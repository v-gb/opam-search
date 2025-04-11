opam-search is a command to which you provide a list of packages and a command.  For
each given package, it fetches its source, runs your command on it, then cleans up.

This is a bit similar to opam-grep, but:

- opam-grep only gives you a boolean for each package, whereas this is more comparable
  to `xargs` or `parallel`: you can output whatever you want, store results in files,
  etc.
- opam-grep is very slow if you need to go through thousands of packages. From memory,
  opam-search may be 5x faster sequentially at fetching sources, and both fetching and
  the user command run in parallel, whereas opam-grep does everything sequentially.
  I've run the ocaml lexer on every ocaml in every package compatible with ocaml 5.2
  (3200 packages) in 50s on a normal laptop with no caching, and 5s with caching of
  the source files. And <1s for a cheaper search like a recursive grep.

Concretely, a use might look like this:

```
opam-search -x 1 -- grep -r --include '*.ml' -F '(*)'
```
