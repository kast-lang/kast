default:
    just --list

lsp-support:
    dune build @ocaml-index --watch