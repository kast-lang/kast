module:

const Drop = [Self] newtype {
    .drop :: &mut Self -> (),
};

const swap = [T] (a :: &mut T, b :: &mut T) => (
    @native "memswap(\(a), \(b), \(T)->size)";
);

const drop = [T] (value :: T) => ();

const forget = [T] (value :: T) => (
    # TODO
);
