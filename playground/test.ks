const std = (
    module:

    const Int32 = @native "Int32";
    const String = @native "String";

    const dbg = (
        module:

        const print = [T] (x :: T) -> () => (
            @native "console.log(\(x))"
        );
    );

    const print = (s :: String) -> () => (
        @native "console.log(\(s))"
    );
);

std.dbg.print(@native "12345" :: std.Int32);
std.print("Hello, world");
