use (import "./rc_str.ks").*;

module:

const OutputT = newtype {
    .color :: Bool,
    .write :: &str -> (),
    .inc_indentation :: () -> (),
    .dec_indentation :: () -> (),
};
const Output = @context OutputT;
const Stdout = @context OutputT;
const Stderr = @context OutputT;

const write_str = (s :: &str) => (
    (@current Output).write(s);
);

const write = (s :: &RcStr) => (
    write_str(RcStr.as_str(s));
);

const write_escaped = (s :: &RcStr) => (
    let escaped = String.escape(s |> RcStr.as_str);
    write_str(&escaped |> as_str);
);

const new_std_output = (write :: &str -> (), .color :: Bool) -> OutputT => (
    let indentation_string = if color then "│   " else "    ";
    new_output(.write, .indentation_string, .color)
);

const output_to_string = (f :: () -> ()) -> String => (
    let mut result = std.StringBuilder.new();
    with Output = new_output(
        .write = s => (
            &mut result |> std.StringBuilder.add_str(s);
        ),
        .indentation_string = "    ",
        .color = false,
    );
    f();
    result |> std.StringBuilder.into_string
);

const new_output = (
    .write :: &str -> (),
    .indentation_string :: &str,
    .color :: Bool,
) -> OutputT => (
    let indentation_string = String.from_str(indentation_string);
    let indentation = std.rc.Rc.new(0);
    let mut started_line = false;
    let write = @move mut s => (
        loop (
            if s == "" then break;
            let i = s |> String.index_of('\n');
            if not started_line and i != 0 then (
                if (@current Output).color then (
                    write("\x1b[");
                    write(ansi.Mode.open_code(:Dim));
                    write("m");
                );
                for _ in 0..indentation^ do (
                    write(&indentation_string |> as_str);
                );
                if (@current Output).color then (
                    write("\x1b[");
                    write(ansi.Mode.close_code(:Dim));
                    write("m");
                );
            );
            if i < 0 then (
                write(s);
                started_line = true;
                break;
            );
            let i = i + 1;
            write(s |> String.substring(0, i));
            s = s |> String.substring(i, String.length(s) - i);
            started_line = false;
        );
    );
    {
        .color,
        .inc_indentation = @move () => (
            indentation^ += 1;
        ),
        .dec_indentation = @move () => (
            indentation^ -= 1;
        ),
        .write,
    }
);

const ansi = (
    module:

    const Mode = newtype (
        | :Bold
        | :Dim
        | :Italic
        | :Under
        | :Blink
        | :Strike
        | :Black
        | :Red
        | :Green
        | :Yellow
        | :Blue
        | :Magenta
        | :Cyan
        | :White
        | :Gray
        | :BlackBg
        | :RedBg
        | :GreenBg
        | :YellowBg
        | :BlueBg
        | :MagentaBg
        | :CyanBg
        | :WhiteBg
        | :GrayBg
    );

    impl Mode as module = (
        module:

        const open_code = (self :: Mode) -> &str => match self with (
            | :Bold => "1"
            | :Dim => "2"
            | :Italic => "3"
            | :Under => "4"
            | :Blink => "5"
            | :Strike => "9"
            | :Black => "30"
            | :Red => "31"
            | :Green => "32"
            | :Yellow => "33"
            | :Blue => "34"
            | :Magenta => "35"
            | :Cyan => "36"
            | :White => "37"
            | :Gray => "90"
            | :BlackBg => "40"
            | :RedBg => "41"
            | :GreenBg => "42"
            | :YellowBg => "43"
            | :BlueBg => "44"
            | :MagentaBg => "45"
            | :CyanBg => "46"
            | :WhiteBg => "47"
            | :GrayBg => "100"
        );

        const close_code = (self :: Mode) -> &str => match self with (
            | :Bold => "22"
            | :Dim => "22"
            | :Italic => "23"
            | :Under => "24"
            | :Blink => "25"
            | :Strike => "29"
            | :Black => "39"
            | :Red => "39"
            | :Green => "39"
            | :Yellow => "39"
            | :Blue => "39"
            | :Magenta => "39"
            | :Cyan => "39"
            | :White => "39"
            | :Gray => "39"
            | :BlackBg => "49"
            | :RedBg => "49"
            | :GreenBg => "49"
            | :YellowBg => "49"
            | :BlueBg => "49"
            | :MagentaBg => "49"
            | :CyanBg => "49"
            | :WhiteBg => "49"
            | :GrayBg => "49"
        );
    );

    const write_code = (code :: &str) => (
        let output = @current Output;
        if output.color then (
            output.write("\x1b[");
            output.write(code);
            # if multiple code can separate with ";"
            output.write("m");
        );
    );

    const open = (mode :: Mode) => (
        write_code(mode |> Mode.open_code);
    );

    const close = (mode :: Mode) => (
        write_code(mode |> Mode.close_code);
    );

    const with_mode = (mode :: Mode, f :: () -> ()) => (
        open(mode);
        f();
        close(mode);
    );
);
