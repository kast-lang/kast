use std.*;

syntax extrapolate <- 1 = seq "...";
impl syntax extrapolate = macro (.seq) => `(
    extrapolate-impl list[$seq]
);

const Seq = () -> int32 with ();

let extrapolate-impl = fn(seq :: list[int32]) -> Seq with () {
    if list_length &seq != 3 then panic "can only extrapolate 3 for now";
    let a, b, c = list_get (&seq, 0), list_get (&seq, 1), list_get (&seq, 2);
    let a, b, c = a^, b^, c^;
    let next = a;
    let next_step = b - a;
    let step_step = (c - b) - (b - a);
    fn (()) -> int32 with () {
        let cur = next;
        next += next_step;
        next_step += step_step;
        cur
    }
};

# dbg `(1, 2, 3 ...);
let seq = (1, 2, 4 ...);

dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
dbg (seq ());
