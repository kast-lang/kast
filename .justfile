default:
    just --list

lsp-support:
    dune build @ocaml-index --watch

dep-graph:
    dune-deps --exclude .direnv | tred | dot -Tpng > dep-graph.png