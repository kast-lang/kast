use (import "./output.ks").*;

module:

const Position = newtype {
    .index :: Int32,
    .line :: Int32,
    .column :: Int32,
};

impl Position as module = (
    module:

    const beginning = () -> Position => {
        .index = 0,
        .line = 0,
        .column = 0,
    };

    const advance = (pos :: &mut Position, c :: Char) => (
        pos^.index += Char.string_encoding_len(c);
        if c == '\n' then (
            pos^.line += 1;
            pos^.column = 0;
        ) else (
            pos^.column += Char.utf16_len(c);
        );
    );

    const print = (self :: Position) => (
        let output = @current Output;
        output.write(to_string(self.line + 1));
        output.write(".");
        output.write(to_string(self.column + 1));
    );
);