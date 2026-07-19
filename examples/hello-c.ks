const Int = @native "Int32";

let add = (a :: Int, b :: Int) -> Int => (
    @native "\(a) + \(b)"
);

let print_Int = (x :: Int) => (
    @native "#include <stdio.h>";
    @native "printf(\"%d\", \(x))";
);

let foo = (a :: Int) => (
    print_Int(0);
);

let create = (start_value :: Int) => (
    let mut x :: Int = start_value;
    () => (
        print_Int(x);
        x = add(x, 1);
    )
);

let print_newline = fn @call "C"() => (
    @native "#include <stdio.h>";
    @native "printf(\"\\n\")";
);

let f = create(6);
f();
f();
print_newline();
