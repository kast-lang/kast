use std.prelude.*;
let a = List.create ();
List.push_back (&a, 1 :: Int32);
List.push_back (&a, 2);
List.push_back (&a, 3);
List.iter (&a, &x => dbg.print x);

use std.collections.Queue;
let queue = Queue.create ();
let n = 1;
for i in 0..1 do (
    Queue.push (&queue, i);
);
for _ in 0..1 do (
    dbg.print <| Queue.pop (&queue);
);
