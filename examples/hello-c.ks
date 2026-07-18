const Int = @native "Int32";

let print_Int = (x ::Int) => (
    @native "#include <stdio.h>";
    @native "printf(\"%d\", \(x))";
);

let mut x :: Int = 0;
let print_x = () => (
    print_Int(x);
);

let print_newline = fn @call "C"() => (
    @native "#include <stdio.h>";
    @native "printf(\"\\n\")";
);

x = 6;
print_x();
x = 7;
print_x();
print_newline();
