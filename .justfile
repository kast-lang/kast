default:
    just --list

test:
    nix flake check

repl:
    cargo run -- repl

run *ARGS:
    cargo run -- run {{ARGS}}

serve:
    cd website && zola serve

asteroids:
    mkdir -p dist
    just run --to-js examples/asteroids/main.ks > dist/asteroids.js
    cp examples/asteroids/index.html dist/index.html
    caddy file-server --listen localhost:8000 --root dist
