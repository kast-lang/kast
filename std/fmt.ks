use collections.ArrayList;

module:

const Write = [Self] newtype {
    .write :: (&mut Self, &str) -> (),
};

impl StringBuilder.t as Write = {
    .write = (self, s) => (
        StringBuilder.add_str(self, s);
    ),
};

const write_to = [T] (output :: &mut T, s :: &str) => (
    (T as Write).write(output, s);
);

const write_impl = (
    output :: Ast,
    fmt :: Ast,
    args :: ArrayList.t[Ast],
) -> Ast => (
    let fmt = match fmt |> Ast.shape with (
        | :String fmt => fmt
        | _ => panic("Expected format string")
    );
    let mut result = `();
    for part in fmt.parts |> ArrayList.into_iter do (
        match part with (
            | :Content { .content, ... } => (
                result = `(
                    $result;
                    write_to($output, &content |> String.as_str);
                );
            )
            | :Interpolate { .value, ... } => (
                result = `(
                    $result;
                    write_to($output, &String.to_string($value) |> String.as_str);
                );
            )
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
    `(include_ast write_impl(output, fmt, fmt_args))
);

const writeln = (args_ast :: Ast) -> Ast => (
    let args :: ArrayList.t[Ast] = args_ast |> Ast.get_comma_separated_list;
    let output = args.[0];
    `(
        write!($args_ast);
        write!($output, "\n");
    )
);

const format = (args :: Ast) -> Ast => `(
    let mut output = StringBuilder.new();
    write!(&mut output, $args);
    output |> StringBuilder.into_string
);

