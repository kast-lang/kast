use std.collections.OrdMap;

let test_a = mut a => (
    let dump_a = () => (
        for &x in &a |> ArrayList.iter do (
            print!("\(x) ");
        );
        println!("");
    );
    # dump_a();
    let dump_freqs = () => (
        let mut freqs = OrdMap.new();
        for &x in &a |> ArrayList.iter do (
            let freq = &mut freqs |> OrdMap.get_or_init(x, () => 0);
            freq^ += 1;
        );

        for &{ .key, .value } in &freqs |> OrdMap.iter do (
            println!("occurences of \(key) = \(value)");
        );
    );

    &mut a |> ArrayList.sort;
    for i in 0..(&a |> ArrayList.length) - 1 do (
        if a.[i] > a.[i + 1] then (
            println!("incorrectly sorted:");
            dump_a();
            panic("sort didnt sort");
        );
    );
);

let test = (n, max) => (
    let mut a = ArrayList.new();
    for _ in 0..n do (
        &mut a |> ArrayList.push_back(std.random.gen_range(.min = 0, .max));
    );
    test_a(a);
);

test_a((
    let mut a = ArrayList.new();
    let s = "8 2 4 6 7 5 3 1";
    for x in s |> String.split(' ') do (
        &mut a |> ArrayList.push_back(parse(x));
    );
    a
));

for _ in 0..1000 do (
    test(std.random.gen_range(.min = 1, .max = 1000), 1000);
);
