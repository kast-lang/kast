module:
const less = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "<") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} < {b})"
    )
);
const less_or_equal = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "<=") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} <= {b})"
    )
);
const equal = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "==") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} = {b})"
    )
);
const not_equal = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native "!=") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} <> {b})"
    )
);
const greater_or_equal = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native ">=") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} >= {b})"
    )
);
const greater = (a :: int32, b :: int32) -> bool => (
    cfg_if (
        | (native "==") (target.name, "interpreter") => (native ">") (a, b)
        | (native "==") (target.name, "ocaml") => native "({a} > {b})"
    )
);
