use collections.ArrayList;

module:

const Formatter = newtype {
    .write_str :: &str -> (),
};

const Display = [Self] newtype {
    .display :: (&Self, &mut Formatter) -> (),
};

impl String as Display = {
    .display = (self, fmt) => (
        fmt^.write_str(self |> String.as_str);
    ),
};

impl &str as Display = {
    .display = (self, fmt) => (
        fmt^.write_str(self^);
    ),
};

const Display_via_to_string = T => `(
    impl T as Display = {
        .display = (self, fmt) => (
            fmt^.write_str(&String.to_string(self^) |> String.as_str);
        ),
    };
);

include_ast Display_via_to_string(Bool);
include_ast Display_via_to_string(Int32);
include_ast Display_via_to_string(UInt32);
include_ast Display_via_to_string(Int64);
include_ast Display_via_to_string(UInt64);
include_ast Display_via_to_string(Float32);
include_ast Display_via_to_string(Float64);
include_ast Display_via_to_string(Char);
include_ast Display_via_to_string(Type);

const Write = [Self] newtype {
    .write :: (&mut Self, &str) -> (),
};

impl Formatter as Write = {
    .write = (self, s) => (
        self^.write_str(s);
    ),
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
) -> Ast => @comptime_only (
    let fmt = match fmt |> Ast.shape with (
        | :String fmt => fmt
        | _ => panic("Expected format string")
    );
    let formatter = `(fmt);
    let mut result = `(
        let output = $output;
        let mut $formatter :: Formatter = {
            .write_str = s => ((typeof (output^)) as Write).write(output, s),
        };
    );
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
                    let value = &$value;
                    ((typeof (value^)) as Display).display(value, &mut $formatter)
                );
            )
        );
    );
    result
);

# The macro!(args) syntax calls a macro fn with args as single arg,
# we need to parse it into (output, fmt, fmt_args)
const write = (args :: Ast) -> Ast => @comptime_only (
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

const writeln = (args_ast :: Ast) -> Ast => @comptime_only (
    let args :: ArrayList.t[Ast] = args_ast |> Ast.get_comma_separated_list;
    let output = args.[0];
    `(
        write!($args_ast);
        write!($output, "\n");
    )
);

const format = (args :: Ast) -> Ast => @comptime_only `(
    let mut output = StringBuilder.new();
    write!(&mut output, $args);
    output |> StringBuilder.into_string
);

