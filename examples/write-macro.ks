use std.Ast;

# For simplicity, lets write to String in the most ineffecient way
const write_to = (output :: &mut String, s :: String) => (
    output^ = output^ + s;
);

# Parse fmt string, write every character directly to output,
# but every occurence of {} will be written to output after being converted to string
const write_impl = (
    output :: Ast,
    fmt :: String,
    args :: ArrayList.t[Ast],
) -> Ast => (
    let mut next_arg_idx = 0;
    let mut result = `();
    let mut i = 0;
    while i < String.length(fmt) do (
        let c = String.at(fmt, i);
        if (
            c == '{'
            and i + 1 < String.length(fmt)
            and String.at(fmt, i + 1) == '}'
        ) then (
            result = `(
                $result;
                write_to($output, to_string($(args.[next_arg_idx])));
            );
            next_arg_idx += 1;
            i += 2;
        ) else (
            result = `(
                $result;
                write_to($output, to_string(c));
            );
            i += 1;
        );
    );
    result
);

# The macro!(args) syntax calls a macro fn with args as single arg,
# we need to parse it into (output, fmt, fmt_args)
const write = (args :: Ast) -> Ast => (
    # get_comma_separated_list is builtin for now
    # ideally would want to have pattern matching for asts I think
    let args :: ArrayList.t[Ast] = args |> Ast.get_comma_separated_list;
    let output = args.[0];
    let fmt = args.[1];
    let mut fmt_args = ArrayList.new();
    for i in 2..ArrayList.length(&args) do (
        &mut fmt_args |> ArrayList.push_back(args.[i]);
    );
    # A little magic: we ast-interpolate only fmt
    # since we want to evaluate it to an actual string
    `(include_ast write_impl(output, $fmt, fmt_args))
);

# Usage of the macro
let mut output :: String = "";
write!(&mut output, "Hello, {}! Here's a random number: {}", "World", 67 :: Int32);
print(output);
