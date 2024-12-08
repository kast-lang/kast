use std.*;

const enumerate = forall[T :: type] {
    fn (gen :: () -> ()) -> () with generator_handler[int32, T] {
        let outer = current generator_handler[int32, T];
        let _ = current generator_handler[T];
        let mut i = 0;
        for item :: T in gen() {
            outer.handle (i, item);
            i += 1;
        };
    }
};

for i :: int32, s :: string in enumerate[string] (() => list_iter list["hello", "world"]) {
    dbg (i, s);
}