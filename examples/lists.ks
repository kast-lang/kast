use std.prelude.*;
let mut a = List.create();
List.push_back(&mut a, 1 :: Int32);
List.push_back(&mut a, 2);
List.push_back(&mut a, 3);
for &x in List.iter(&a) do (
    dbg.print(x);
);

use std.collections.Queue;
let mut queue = Queue.create();
for i in 0..1 do (
    Queue.push(&mut queue, i);
);
for _ in 0..1 do (
    dbg.print <| Queue.pop(&mut queue);
);
