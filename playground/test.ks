if std.iter.find({ .iter = (1..10).iter }, x => x % 2 == 0) is :Some x then (
    print(to_string(x));
);
