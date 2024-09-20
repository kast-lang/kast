test TEST:
    just run examples/{{TEST}}.ks

repl:
    just run --repl

run *ARGS:
    cargo run -- {{ARGS}}

serve:
    cd website && zola serve

asteroids:
    mkdir -p dist
    just run --to-js examples/asteroids/main.ks > dist/asteroids.js
    cp examples/asteroids/index.html dist/index.html
    caddy file-server --listen localhost:8000 --root dist
