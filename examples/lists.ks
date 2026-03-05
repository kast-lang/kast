let mut a = ArrayList.new();
ArrayList.push_back(&mut a, 1 :: Int32);
ArrayList.push_back(&mut a, 2);
ArrayList.push_back(&mut a, 3);
for &x in ArrayList.iter(&a) do (
    dbg.print(x);
);

use std.collections.Queue;
let mut queue = Queue.new();
for i in 0..1 do (
    Queue.push(&mut queue, i);
);
for _ in 0..1 do (
    dbg.print <| Queue.pop(&mut queue);
);
