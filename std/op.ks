module:
const neg = [T] (x :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "unary -") x
);
const pos = [T] (x :: T) -> T => (
    x
);
const add = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "+") (a, b)
        | target.name == "ocaml" => @native "({a} + {b})"
        | target.name == "javascript" => (@native "Kast.op.add") (a, b)
    )
);
const sub = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "-") (a, b)
        | target.name == "ocaml" => @native "({a} - {b})"
        | target.name == "javascript" => (@native "Kast.op.sub") (a, b)
    )
);
const mul = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "*") (a, b)
        | target.name == "ocaml" => @native "({a} * {b})"
        | target.name == "javascript" => (@native "Kast.op.mul") (a, b)
    )
);
const div = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "/") (a, b)
        | target.name == "ocaml" => @native "({a} / {b})"
    )
);
const rem = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "%") (a, b)
);
const bit_and = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_and") (a, b)
);
const bit_or = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_or") (a, b)
);
const bit_xor = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_xor") (a, b)
);
const bit_not = [T] (x :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_not") x
);
const bit_shift_left = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_shift_left") (a, b)
);
const bit_shift_right = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_shift_right") (a, b)
);
