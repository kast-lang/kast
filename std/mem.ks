module:

const Drop = [Self] newtype {
    .drop :: &mut Self -> (),
};

const drop = [T] (value :: T) => ();
