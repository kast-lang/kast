use std.*;

const f = forall[T :: type] {
    () => (
        let value = current T;
        dbg value;
    )
};

let text :: string = "hello, world";
with text;
f[string]();
