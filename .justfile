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

profile *args:
    perf record --call-graph dwarf _build/install/default/bin/kast {{args}}
    perf script -F +pid > data.perf
    echo "now load data.perf into https://profiler.firefox.com/"

lsp-stress-test:
    python lsp-stress-test/main.py 2>/dev/null | kast lsp >/dev/null

minikast path *args:
    kast compile --target minikast-js {{path}} > target/compiled.mks
    node ../self-host/target/kast.mjs --color false mini compile \
        --js-runtime _build/default/src/transpiler/javascript/runtime.js \
        target/compiled.mks \
        > target/compiled.mjs
    node target/compiled.mjs {{args}}

test-aoc *args:
    KAST_STD=$(pwd)/std kast ${AOC:-~/projects/aoc2025/test.ks} {{args}}

run-js path *args:
    kast run --format prettier --target javascript {{path}} {{args}}

bench *args:
    kast compile \
        --output target/compiled.mjs \
        --target javascript \
        {{args}}
    hyperfine "node target/compiled.mjs"