impl syntax (@context ty) = `(
    (@native "create_context_type")($ty)
);

const std = (
    module:

    const Int = @native "Int32";
    const Float = @native "Float64";
    const String = @native "String";

    const print_String = (s :: String) => (
        @native "print_String(\(s))";
    );

    const PanicHandler = @context type (String -> ());

    const default_panic_handler = (s :: String) -> () => (
        print_String("PANIC: ");
        print_String(s);
        @native "#include <stdlib.h>";
        @native "exit(-1)";
        @native "#unreachable"
    );

    const panic = (s :: String) => (
        (@current PanicHandler)(s)
    );

    const add = [T] (a :: T, b :: T) -> T => (
        @native "\(a) + \(b)"
    );

    const print_Int = (x :: Int) => (
        @native "#include <stdio.h>";
        @native "printf(\"%d\", \(x))";
    );

    const print_Float = (x :: Float) => (
        @native "#include <stdio.h>";
        @native "printf(\"%lf\", \(x))";
    );

    const Option = (
        module:
        
        const t = [T] newtype (
            | :None
            | :Some T
        );
    );
);

use std.*;
with PanicHandler = std.default_panic_handler;

print_String("Hello, C! From Kast 🦄\n");

let foo = (a :: Int) => (
    print_Int(0);
);

let create = (start_value :: Int) => (
    let mut x :: Int = start_value;
    () => (
        print_Int(x);
        x = add(x, 3);
    )
);

let print_newline = fn @call "C" () => (
    @native "#include <stdio.h>";
    @native "printf(\"\\n\")";
);

let f = create(6);
f();
f();
print_newline();

let a :: Float = 0.123;
print_Float(add(a, 123));
print_newline();

(
    # with PanicHandler = [T] (s => print_String);
    panic("Not really a panic\n");
);

panic("I PANIKED");
