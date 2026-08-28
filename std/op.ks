module:

const neg = [T] (x :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "unary -")(x)
    | target.name == "c" => @native "-\(x)"
    | target.name == "javascript" => (@native "Kast.op.neg")(x)
);
const pos = [T] (x :: T) -> T => (
    x
);

const Add = [Self] newtype {
    .add :: (Self, Self) -> Self,
};

impl Int32 as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "\(a) + \(b)"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

impl UInt32 as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "\(a) + \(b)"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

impl Int64 as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "\(a) + \(b)"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

impl UInt64 as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "\(a) + \(b)"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

impl Float64 as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "\(a) + \(b)"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

impl String as Add = {
    .add = (a, b) => @cfg (
        | target.name == "interpreter" => (@native "+")(a, b)
        | target.name == "c" => @native "String_concat(\(a), \(b))"
        | target.name == "javascript" => @native "\(a)+\(b)"
    )
};

const add = [T] (a :: T, b :: T) -> T => (
    (T as Add).add(a, b)
);
const sub = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "-")(a, b)
        | target.name == "c" => @native "\(a) - \(b)"
        | target.name == "javascript" => (@native "Kast.op.sub")(a, b)
    )
);
const mul = [T] (a :: T, b :: T) -> T => (
    @cfg (
        | target.name == "interpreter" => (@native "*")(a, b)
        | target.name == "c" => @native "\(a) * \(b)"
        | target.name == "javascript" => (@native "Kast.op.mul")(a, b)
    )
);
const div = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "/")(a, b)
    | target.name == "c" => @native "\(a) / \(b)"
    | target.name == "javascript" => (@native "Kast.op.div_temp")(T, a, b)
);
const rem = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "%")(a, b)
    | target.name == "c" => @native "\(a) % \(b)"
    | target.name == "javascript" => (@native "Kast.op.rem")(a, b)
);
const bit_and = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_and")(a, b)
    | target.name == "c" => @native "\(a) & \(b)"
    | target.name == "javascript" => (@native "Kast.op.bit_and")(a, b)
);
const bit_or = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_or")(a, b)
    | target.name == "c" => @native "\(a) | \(b)"
    | target.name == "javascript" => (@native "Kast.op.bit_or")(a, b)
);
const bit_xor = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_xor")(a, b)
    | target.name == "c" => @native "\(a) ^ \(b)"
    | target.name == "javascript" => (@native "Kast.op.bit_xor")(a, b)
);
const bit_not = [T] (x :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_not")(x)
    | target.name == "c" => @native "~\(x)"
    | target.name == "javascript" => (@native "Kast.op.bit_not")(x)
);
const bit_shift_left = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_shift_left")(a, b)
    | target.name == "c" => @native "\(a) << \(b)"
    | target.name == "javascript" => (@native "Kast.op.bit_shift_left")(a, b)
);
const bit_shift_right = [T] (a :: T, b :: T) -> T => @cfg (
    | target.name == "interpreter" => (@native "bit_shift_right")(a, b)
    | target.name == "c" => @native "\(a) >> \(b)"
    | target.name == "javascript" => (@native "Kast.op.bit_shift_right")(a, b)
);
