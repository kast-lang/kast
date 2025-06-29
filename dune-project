(lang dune 3.16)

(name kast)

(version 0.3.0)

(package
 (name kast)
 (depends
  ocaml
  ppx_deriving
  ppx_blob
  ; https://github.com/c-cube/linol/issues/57
  (linol
   (= 0.9))
  linol-lwt
  js_of_ocaml
  js_of_ocaml-compiler
  js_of_ocaml-ppx))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
