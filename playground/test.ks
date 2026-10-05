let mut a = ArrayList.new[Int32]();
&mut a |> ArrayList.push_back(1);
let mut b = ArrayList.new();
&mut b |> ArrayList.push_back(1);
println!("\(std.repr.structurally_equal(&a, &b))");
