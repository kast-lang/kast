const Foo = newtype ( :A | :B );

let foo :: Option.t[Foo] = :Some :B;

match foo with (
    | :Some :A => print("A")
    | :Some :B => print("B")
    | :None => print("None")
);
