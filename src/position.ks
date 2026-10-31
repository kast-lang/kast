use (import "./output.ks").*;

module:

const Position = newtype {
    .string_encoding_index :: Int32,
    .line :: Int32,
    .column :: PositionColumn,
};

impl Position as std.cmp.Ord = {
    .compare = (a, b) => with_return (
        match std.cmp.default_compare(&a^.line, &b^.line) with (
            | :Equal => ()
            | other => return other
        );
        std.cmp.default_compare(&a^.column.string_encoding, &b^.column.string_encoding)
    ),
};

const PositionColumn = newtype {
    .string_encoding :: Int32,
    .display_width :: Int32,
};

impl PositionColumn as module = (
    module:

    const zero = () -> PositionColumn => {
        .string_encoding = 0,
        .display_width = 0,
    };
);

impl Position as module = (
    module:

    const beginning = () -> Position => {
        .string_encoding_index = 0,
        .line = 0,
        .column = PositionColumn.zero(),
    };

    const advance = (pos :: &mut Position, c :: Char) => (
        pos^.string_encoding_index += Char.string_encoding_len(c);
        if c == '\n' then (
            pos^.line += 1;
            pos^.column = PositionColumn.zero();
        ) else (
            # TODO should be unicode width?
            # https://www.unicode.org/reports/tr11/
            pos^.column.display_width += Char.utf16_len(c);
            pos^.column.string_encoding += Char.string_encoding_len(c);
        );
    );

    const advance_copy = (pos :: Position, c :: Char) -> Position => (
        let mut new_pos = pos;
        advance(&mut new_pos, c);
        new_pos
    );

    const print = (self :: &Position) => (
        let output = @current LockedOutput;
        output^.write(&to_string(self^.line + 1) |> as_str);
        output^.write(".");
        output^.write(&to_string(self^.column.display_width + 1) |> as_str);
    );
);
