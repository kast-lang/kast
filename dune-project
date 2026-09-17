(lang dune 3.16)

(name kast)

(version 0.3.0)

(package
 (name kast)
 (depends
  (ocaml
   (and
    (>= 5.4)
    (< 5.5)))
  (ppx_deriving
   (and
    (>= 6)
    (< 7)))
  (ppx_getenv
   (and
    (>= 2)
    (< 3)))
  (linol
   (and
    (>= 0.10)
    (< 0.11)))
  (linol-eio
   (and
    (>= 0.10)
    (< 0.11)))
  (eio_main
   (and
    (>= 1)
    (< 2)))
  (js_of_ocaml
   (and
    (>= 6.2)
    (< 6.3)))
  (js_of_ocaml-compiler
   (and
    (>= 6.2)
    (< 6.3)))
  (js_of_ocaml-ppx
   (and
    (>= 6.2)
    (< 6.3)))
  (promise_jsoo
   (and
    (>= 0.4)
    (< 0.5)))
  (memtrace
   (and
    (>= 0.2)
    (< 0.3)))
  (uri
   (and
    (>= 4)
    (< 5)))))

(generate_opam_files)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
