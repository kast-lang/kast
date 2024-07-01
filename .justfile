test TEST:
    just run examples/syntax examples/{{TEST}}

run *ARGS:
    _build/default/bin/main.exe {{ARGS}}
    # dune exec playground -- {{ARGS}}
