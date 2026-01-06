module:
const less = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "<") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} < {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.less") (a, b)
    )
);
const less_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "<=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} <= {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.less_or_equal") (a, b)
    )
);
const equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "==") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} = {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.equal") (a, b)
    )
);
const not_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native "!=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} <> {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.not_equal") (a, b)
    )
);
const greater_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native ">=") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} >= {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.greater_or_equal") (a, b)
    )
);
const greater = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==") (target.name, "interpreter") => (@native ">") (a, b)
        | (@native "==") (target.name, "ocaml") => @native "({a} > {b})"
        | (@native "==") (target.name, "javascript") => (@native "Kast.cmp.greater") (a, b)
    )
);
