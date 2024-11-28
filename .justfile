default:
    just --list

test *ARGS:
    cargo test --release --no-default-features -- {{ARGS}}

repl *ARGS:
    cargo run --no-default-features -- repl {{ARGS}}

run *ARGS:
    cargo run --no-default-features -- run {{ARGS}}

serve:
    cd website && zola serve

asteroids:
    mkdir -p dist
    just run --to-js examples/asteroids/main.ks > dist/asteroids.js
    cp examples/asteroids/index.html dist/index.html
    caddy file-server --listen localhost:8000 --root dist
