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

[arg("no-std", long="no-std", value="--no-std")]
minikast path no-std="" *args:
    #!/usr/bin/env bash
    set -e
    mks=$(realpath target/compiled.mks)
    mjs=$(realpath target/compiled.mjs)
    runtime_js=$(realpath _build/default/src/transpiler/javascript/runtime.js)
    kast compile {{no-std}} --target minikast-js {{path}} > "$mks"
    pushd ../self-host
    node target/kast.mjs --color false mini compile \
        --js-runtime "$runtime_js" \
        "$mks" \
        > "$mjs"
    popd
    node "$mjs" {{args}}

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