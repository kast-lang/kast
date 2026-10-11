module:

const Foo = newtype {
    .field :: &Foo,
};

let foo :: &Foo = @native "haha";
