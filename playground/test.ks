let mut a :: ArrayList.t[Int32] = ArrayList.new();

for i in 0..10 do (
    &mut a |> ArrayList.push_back(i);
);

for x in a |> ArrayList.into_iter do (
    print(to_string(x));
);

# print(ArrayList.to_string[Int32](&a, &x => to_string[Int32](x)));