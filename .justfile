default:
    just --list

lsp-support:
    dune build @ocaml-index src/kast/js/kast_js.bc.js --watch
    # dune build @ocaml-index --watch

dep-graph:
    dune-deps --exclude .direnv | tred | dot -Tpng > dep-graph.png

js-playground:
    dune build @kast_js_playground
    caddy file-server --listen 127.0.0.1:8080 --root _build/default/src/kast/js