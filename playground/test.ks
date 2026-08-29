const Pair = newtype {
    .a :: Int32,
    .b :: Int32,
};

let mut list = ArrayList.new[Pair]();
&mut list |> ArrayList.push_back({ .a = 1, .b = 2 });
