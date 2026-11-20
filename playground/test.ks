use std.rc.Rc;

const C = @context Rc.t[Int32];

let f = (
    with C = Rc.new(123);

    let mut c = @context;
    @move () => (
        let &@context = &mut c;
        dbg.print(@current C);
    )
);
f();
