use std.collections.OrdMap;

let mut map = OrdMap.new[Int32, Int32]();
&mut map |> OrdMap.add(1, 2);

let at_1 = OrdMap.get_mut(&mut map, 1) |> Option.unwrap;
at_1^ = 3;

let at_1 = OrdMap.get(&map, 1) |> Option.unwrap;
print(to_string(at_1^));
