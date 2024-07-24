test TEST:
    just run std/lib.kast examples/{{TEST}}.kast

repl:
    just run std/lib.kast

run *ARGS:
    rlwrap _build/default/bin/main.exe {{ARGS}}
    # dune exec playground -- {{ARGS}}

serve:
    cd website && zola serve

dune:
    dune build --watch
