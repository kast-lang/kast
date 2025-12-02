module:
const add = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | (@native "==") (target.name, "interpreter") => (@native "+") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} + {b})"
    )
);
const sub = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | (@native "==") (target.name, "interpreter") => (@native "-") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} - {b})"
    )
);
const mul = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | (@native "==") (target.name, "interpreter") => (@native "*") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} * {b})"
    )
);
const div = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | (@native "==") (target.name, "interpreter") => (@native "/") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} / {b})"
    )
);
