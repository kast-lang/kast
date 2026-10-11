module:

const Foo = newtype {
    .field :: &Foo,
};

let foo :: Box[Foo] = @native "haha";
