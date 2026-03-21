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
        # TODO proper copy in kast
        pos^ = { ...pos^ };
        pos^.index += Char.string_encoding_len(c);
        if c == '\n' then (
            pos^.line += 1;
            pos^.column = 0;
        ) else (
            # TODO should be unicode width?
            # https://www.unicode.org/reports/tr11/
            pos^.column += Char.utf16_len(c);
        );
    );

    const advance_copy = (pos :: Position, c :: Char) -> Position => (
        let mut new_pos = pos;
        advance(&mut new_pos, c);
        new_pos
    );

    const print = (self :: Position) => (
        let output = @current Output;
        output.write(to_string(self.line + 1));
        output.write(".");
        output.write(to_string(self.column + 1));
    );
);