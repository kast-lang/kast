test TEST:
    just run examples/syntax examples/{{TEST}}

run *ARGS:
    dune exec playground -- {{ARGS}}
