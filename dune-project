(lang dune 3.16)

(name kast)

(version 0.3.0)

(package
 (name kast)
 (depends
  ocaml
  ppx_deriving
  ppx_getenv
  linol
  linol-eio
  eio_main
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx
  promise_jsoo
  memtrace
  uri))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
