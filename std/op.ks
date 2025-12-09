module:
const neg = [T] (x :: T) -> T => cfg_if (
    | target.name == "interpreter" => (@native "unary -") x
);
const pos = [T] (x :: T) -> T => (
    x
);
const add = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | target.name == "interpreter" => (@native "+") (a, b)
        | target.name == "ocaml" => @native "({a} + {b})"
    )
);
const sub = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | target.name == "interpreter" => (@native "-") (a, b)
        | target.name == "ocaml" => @native "({a} - {b})"
    )
);
const mul = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | target.name == "interpreter" => (@native "*") (a, b)
        | target.name == "ocaml" => @native "({a} * {b})"
    )
);
const div = [T] (a :: T, b :: T) -> T => (
    cfg_if (
        | target.name == "interpreter" => (@native "/") (a, b)
        | target.name == "ocaml" => @native "({a} / {b})"
    )
);
const rem = [T] (a :: T, b :: T) -> T => cfg_if (
    | target.name == "interpreter" => (@native "%") (a, b)
);
