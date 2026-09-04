module:

const Int32 = @native "Int32";

const Into = [T] [Self] newtype {
    .into :: Self -> T,
};

impl Int32 as Into[Int32] = {
    .into = x => x,
};
