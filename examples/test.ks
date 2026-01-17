const m = (
    module:
    let plus_one = (n :: UInt32) -> UInt32 => (
        n + (@eval (0 + 1))
    );
);

dbg.print(m.plus_one(123));
