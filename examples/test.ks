const Foo = newtype (
    | :Variant1 Int32
);

impl Foo as String.FromString = (
    .from_string = s => (
        :Variant1 (s |> parse)
    ),
);

dbg.print("123" |> parse :: Foo);
