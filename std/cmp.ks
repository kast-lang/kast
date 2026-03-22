module:
const less = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "<")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} < {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.less")(a, b)
    )
);
const less_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "<=")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} <= {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.less_or_equal")(a, b)
    )
);
const equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "==")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} = {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.equal")(a, b)
    )
);
const not_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native "!=")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} <> {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.not_equal")(a, b)
    )
);
const greater_or_equal = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native ">=")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} >= {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.greater_or_equal")(a, b)
    )
);
const greater = [T] (a :: T, b :: T) -> Bool => (
    @cfg (
        | (@native "==")(target.name, "interpreter") => (@native ">")(a, b)
        | (@native "==")(target.name, "ocaml") => @native "({a} > {b})"
        | (@native "==")(target.name, "javascript") => (@native "Kast.cmp.greater")(a, b)
    )
);

const Compare = [T] type ((T, T) -> Ordering);

const Ordering = newtype (
    | :Less
    | :Equal
    | :Greater
);

impl Ordering as module = (
    module:

    const is_less = (self :: Ordering) -> Bool => match self with (
        | :Less => true
        | _ => false
    );

    const is_less_or_equal = (self :: Ordering) -> Bool => match self with (
        | :Greater => false
        | _ => true
    );

    const equal = (self :: Ordering) -> Bool => match self with (
        | :Equal => true
        | _ => false
    );

    const not_equal = (self :: Ordering) -> Bool => match self with (
        | :Equal => false
        | _ => true
    );

    const greater_or_equal = (self :: Ordering) -> Bool => match self with (
        | :Less => false
        | _ => true
    );

    const greater = (self :: Ordering) -> Bool => match self with (
        | :Greater => true
        | _ => false
    );
);

const default_compare = [T] (a :: T, b :: T) -> Ordering => (
    if a < b then (
        :Less
    ) else if a == b then (
        :Equal
    ) else (
        :Greater
    )
);
