use std.*;

dbg <| time.now ();
let start = time.now();

let mut i :: int32 = 0;
let mut a :: list[int32] = list[];
while i < 100 {
    a = list_push(a, i);
    i += 1;
};

dbg a;

dbg (time.now() - start);
