use std.*;

const mod = struct (
    const inner = struct (
        const id = forall[T] {
            (x :: T) => x
        };
    );
);

const id = forall[T] { () => () };
dbg id[int32];
