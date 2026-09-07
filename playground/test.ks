module:

const Foo = newtype (
    | :Unit
    | :Pair { Foo, Foo }
    | :List ArrayList.t[Foo]
);

let foo :: Foo = :Pair { :Unit, :Pair { :Unit, :Unit } };

