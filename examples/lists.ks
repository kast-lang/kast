use std.*;

let x :: list[int32] = list[2, 4, 1, 3];

let dbg_list = forall[T] {
    fn (x :: list[T]) {
        for elem :: T in list_iter[_] x {
            dbg elem;
        };
    }
};
dbg_list x;
