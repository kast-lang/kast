default:
    just --list

testjs *ARGS:
    TEST_ENV=nodejs RUST_MIN_STACK=33554432 cargo test --workspace --no-default-features -- {{ARGS}}
testi *ARGS:
    RUST_MIN_STACK=33554432 cargo test --workspace --no-default-features -- {{ARGS}}
test *ARGS:
    just testi --include-ignored {{ARGS}}

repl *ARGS:
    just run repl {{ARGS}}

run *ARGS:
    cargo run --no-default-features -- {{ARGS}}

serve:
    cd website && zola serve

asteroids:
    mkdir -p dist
    just run --to-js examples/asteroids/main.ks > dist/asteroids.js
    cp examples/asteroids/index.html dist/index.html
    caddy file-server --listen localhost:8000 --root dist
