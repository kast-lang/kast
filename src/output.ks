module:

const OutputT = newtype {
    .write :: String -> (),
};
const Output = @context OutputT;

const stdout = () -> OutputT => (
    let mut buffer = "";
    {
        .write = s => (
            buffer += s;
            loop (
                let i = buffer |> String.index_of('\n');
                if i < 0 then break;
                std.io.print(buffer |> String.substring(0, i));
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

const write = (s :: String) => (
    (@current Output).write(s);
);
