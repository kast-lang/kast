unwindable block (
    with std.PanicHandler = {
        .handle = s => (
            print(s);
            unwind block ()
        ),
    };
    panic[_]("i panicked");
    panic("unreachable");
);
print("continued");
