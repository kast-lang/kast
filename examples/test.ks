let mut list :: List.t[type (.x :: Int32, .y :: Int64)] = List.create();
List.push_back(&mut list, (.x = 1, .y = 2));

for &(.x, .y) in List.iter(&list) do (
    dbg.print(x);
);


dbg.print(0x123 :: Int32);