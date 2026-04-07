const std = (
    module:

    const String = @native "String";

    const print = (s :: String) -> () => (
        @native "console.log(\(s))"
    );
);

std.print("Hello, world");
