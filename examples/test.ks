const m = (
    module:
    let plus_one = (n :: UInt32) -> UInt32 => (
        n + (@eval (std.op.add(1, 0)))
    );
);

dbg.print(m.plus_one(123));
