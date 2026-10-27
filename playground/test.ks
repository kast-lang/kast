use std.rc.Rc;

const Ctx = @context Int32;

(
    with Ctx = 123;
    dbg.print(@current Ctx);
);
dbg.print(@current Ctx);

