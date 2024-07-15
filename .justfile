test TEST:
    just run examples/syntax.kast examples/{{TEST}}.kast

run *ARGS:
    rlwrap _build/default/bin/main.exe {{ARGS}}
    # dune exec playground -- {{ARGS}}
