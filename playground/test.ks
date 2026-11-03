use std.collections.OrdMap;

let mut a = ArrayList.new();

let dump_a = () => (
    for &x in &a |> ArrayList.iter do (
        print!("\(x) ");
    );
    println!("");
);

for _ in 0..10 do (
    &mut a |> ArrayList.push_back(std.random.gen_range(.min = 0, .max = 100));
);

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

dump_a();
dump_freqs();
&mut a |> ArrayList.sort;
dump_a();
dump_freqs();


