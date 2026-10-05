module:

for c in String.iteri_rev("h🦄i") do (
    dbg.print(c);
);

const Foo = newtype {
    Float64,
    String,
    .s2 :: &str,
    .x :: Option.t[UInt64],
    .none :: Option.t[UInt64],
    .next :: ArrayList.t[Box[type (&Foo)]],
};

let other_foo :: Foo = {
    0,
    String.from_str("a"),
    .x = :None,
    .s2 = "yo",
    .none = :Some 1,
    .next = ArrayList.new(),
};

let foo :: Foo = {
    1.69,
    String.from_str("owned"),
    .x = :Some 2,
    .s2 = "not owned",
    .none = :None,
    .next = (
        let mut list = ArrayList.new();
        &mut list |> ArrayList.push_back(Box_new(&other_foo));
        list
    ),
};
dbg.print(foo);

let mut a = ArrayList.new[Int32]();
&mut a |> ArrayList.push_back(1);
let mut b = ArrayList.new();
&mut b |> ArrayList.push_back(1);
println!("\(std.repr.structurally_equal(&a, &b))");
