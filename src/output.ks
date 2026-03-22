module:

const OutputT = newtype {
    .write :: String -> (),
    .inc_indentation :: () -> (),
    .dec_indentation :: () -> (),
};
const Output = @context OutputT;

const stdout = () -> OutputT => (
    let mut buffer = "";
    let mut indentation = 0;
    {
        .inc_indentation = () => (
            indentation += 1;
        ),
        .dec_indentation = () => (
            indentation -= 1;
        ),
        .write = s => (
            buffer += s;
            loop (
                let i = buffer |> String.index_of('\n');
                if i < 0 then break;
                let line = buffer |> String.substring(0, i);
                let mut s = "\x1b[" + ansi.Mode.open_code(:Dim) + "m";
                for _ in 0..indentation do (
                    s += ("│   ");
                );
                s += "\x1b[" + ansi.Mode.close_code(:Dim) + "m";
                std.io.print(s + line);
                let i = i + 1;
                buffer = String.substring(
                    buffer,
                    i,
                    String.length(buffer) - i,
                );
            );
        ),
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
        
        const open_code = (self :: Mode) -> String => match self with (
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
        
        const close_code = (self :: Mode) -> String => match self with (
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
    
    const write_code = (code :: String) => (
        let output = @current Output;
        output.write("\x1b[");
        output.write(code);
        # if multiple code can separate with ";"
        output.write("m");
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
