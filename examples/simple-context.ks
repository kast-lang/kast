use std.*;

const f = forall[T :: type] {
    () => (
        let value = current string;
        dbg value;
    )
};

let text :: string = "hello, world";
with text;
f[string]();
