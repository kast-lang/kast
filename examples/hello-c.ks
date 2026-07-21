const Int = @native "Int32";
const String = @native "String";

const print_String = (s :: String) => (
    @native "print_String(\(s))";
);

const PanicHandler = @context type (String -> ());

const default_panic_handler = (s :: String) => (
    print_String("PANIC: ");
    print_String(s);
    @native "#include <stdlib.h>";
    @native "exit(-1)";
);

with PanicHandler = default_panic_handler;

const panic = (s :: String) => (
    (@current PanicHandler)(s);
);

let add = (a :: Int, b :: Int) -> Int => (
    @native "\(a) + \(b)"
);

let print_Int = (x :: Int) => (
    @native "#include <stdio.h>";
    @native "printf(\"%d\", \(x))";
);

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

(
    with PanicHandler = print_String;
    panic("Not really a panic\n");
);

panic("I PANIKED");
