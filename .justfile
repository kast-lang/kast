test TEST:
    just run examples/{{TEST}}.ks

repl:
    just run --repl

run *ARGS:
    rlwrap _build/default/bin/main.exe {{ARGS}}
    # dune exec kast -- {{ARGS}}

serve:
    cd website && zola serve

dune:
    dune build --watch
