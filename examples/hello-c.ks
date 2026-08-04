const Int = Int32;
const Float = Float64;

const display = [T] (value :: T) => (
    print(to_string(value));
);

const print_Int = display[Int];
const print_Float = display[Float];

print("Hello, C! From Kast 🦄\n");

let foo = (a :: Int) => (
    print_Int(0);
);

let create = (start_value :: Int) => (
    let mut x :: Int = start_value;
    () => (
        print_Int(x);
        x += 3;
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
print_Float(a + 123);
print_newline();

const fib = (
    module:

    const fib = [T] (one :: T, two :: T, n :: T) -> T => (
        if n < two then (
            one
        ) else (
            fib[_](one, two, n - one) + fib[_](one, two, n - two)
        )
    );

).fib;

# print_String("fib(10) = ");
# print_Int(fib(1, 2, 10));
# print_String("\n");

(
    # with PanicHandler = [T] (s => print_String);
    panic("Not really a panic\n");
);

# panic("I PANIKED");

let std = std;
