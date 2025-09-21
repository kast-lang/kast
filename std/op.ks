module:
const add = (a :: int32, b :: int32) -> int32 => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "+") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} + {b})"
    )
);
const sub = (a :: int32, b :: int32) -> int32 => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "-") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} - {b})"
    )
);
const mul = (a :: int32, b :: int32) -> int32 => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "*") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} * {b})"
    )
);
const div = (a :: int32, b :: int32) -> int32 => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "/") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} / {b})"
    )
);
