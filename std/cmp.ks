module:
const less = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "<") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} < {b})"
    )
);
const less_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "<=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} <= {b})"
    )
);
const equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "==") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} = {b})"
    )
);
const not_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "!=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} <> {b})"
    )
);
const greater_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native ">=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} >= {b})"
    )
);
const greater = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native ">") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} > {b})"
    )
);
