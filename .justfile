default:
    echo Hello there!
    just --list

[arg("continuous", long="continuous", value="--continuous")]
build continuous="":
    mkdir -p target
    # flock --exclusive target 
    time ${KAST_BIN:-kast-bootstrap} compile \
        {{continuous}} \
        --js-ref-vars false \
        --async always \
        --use-numbers-instead-of-symbols false \
        --target js \
        --output target/kast.mjs \
        src/cli/_main.ks

raylib-to-c:
    kast mini \
        compile --target c \
        --prepend kast_path/mini/backends/c/runtime.c \
        kast_path/mini/backends/c/runtime.mks \
        tests/raylib/bindings.mks \
        tests/raylib/main.mks \
        > target/compiled.c

raylib *args:
    just raylib-to-c
    clang target/compiled.c -o target/compiled -lraylib -lgc
    ./target/compiled {{args}}

raylib-web *args:
    just raylib-to-c
    emcc target/compiled.c \
        $RAYLIB_WEB \
        -o target/index.html \
        -I. -I $RAYLIB/include \
        -Os \
        -s USE_GLFW=3 \
        -s ASYNCIFY \
        --preload-file logo.png \
        -s TOTAL_STACK=64MB \
        -s INITIAL_MEMORY=128MB \
        -s ASSERTIONS \
        -DPLATFORM_WEB
    caddy file-server --listen 127.0.0.1:8081 --root target

#--shell-file ../shell.html \

compile-c path:
    kast mini \
        compile --target c \
        --prepend kast_path/mini/backends/c/runtime.c \
        tests/mini/runtime/c.mks \
        kast_path/mini/backends/c/runtime.mks \
        {{path}} \
        > target/compiled.c
    gcc target/compiled.c \
        -Ideps/minicoro \
        -lgc \
        -o target/compiled \
        -ggdb \
        -fanalyzer \
        -fsanitize=address \
        -fsanitize=undefined 

c path *args:
    just compile-c {{path}}
    ./target/compiled {{args}}

js path *args:
    kast mini \
        compile --target js \
        --prepend tests/mini/runtime/js.js \
        tests/mini/runtime/js.mks \
        src/mini/backends/javascript/runtime.mks \
        {{path}} \
        > target/compiled.mjs
    node target/compiled.mjs {{args}}

watch:
    #!/usr/bin/env bash
    trap 'kill $(jobs -p); exit' INT
    inotifywait -m -r -e modify,create,delete,moved_to src |
    while read -r directory events filename; do
        echo "Change detected: $filename ($events). Rebuilding..."
        just build
    done &
    just build
    wait

test:
    just build
    fd --type file --extension ks --exec-batch kast tokenize > /dev/null
    kast parse-syntax-rules tests/syntax/simple.ks > /dev/null
    kast parse-syntax-rules kast_path/*/syntax.ks > /dev/null
    kast parse-json tests/json/*.json > /dev/null
    kast parse-json --use-kast-parser tests/json/*.json > /dev/null
    fd --exclude '**/doesnt-parse/*' --type file --extension ks --exec-batch kast parse > /dev/null
    fd --exclude '**/doesnt-parse/*' --type file --extension ks --exec-batch kast mini parse > /dev/null

lsp-stress-test:
    kast-bootstrap --target js lsp-stress-test/main.ks | kast lsp
