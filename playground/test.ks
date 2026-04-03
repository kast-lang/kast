const Context = @context Int32;

const f = () => (
    dbg.print(@current Context);
);

with Context = 123;
f();
(
    f();
    with Context = 456;
    f();
);
f();