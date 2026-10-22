use (import "./position.ks").*;
use (import "./rc_str.ks").*;

module:

const Reader = newtype {
    .contents :: RcStr,
    .position :: Position,
};

impl Reader as module = (
    module:

    const new = (contents :: RcStr) -> Reader => {
        .contents,
        .position = Position.beginning(),
    };

    const prev = (reader :: &Reader) -> Option.t[Char] => (
        if reader^.position.string_encoding_index == 0 then (
            :None
        ) else (
            :Some (&reader^.contents |> RcStr.at(reader^.position.string_encoding_index - 1))
        )
    );

    const peek = (reader :: &Reader) -> Option.t[Char] => (
        if reader^.position.string_encoding_index < &reader^.contents |> RcStr.length then (
            :Some (&reader^.contents |> RcStr.at(reader^.position.string_encoding_index))
        ) else (
            :None
        )
    );

    const peek2 = (reader :: &Reader) -> Option.t[Char] => (
        match peek(reader) with (
            | :Some peek => (
                let next_index = reader^.position.string_encoding_index + Char.string_encoding_len(peek);
                if next_index < &reader^.contents |> RcStr.length then (
                    :Some (&reader^.contents |> RcStr.at(next_index))
                ) else (
                    :None
                )
            )
            | :None => :None
        )
    );

    const advance = (reader :: &mut Reader) => (
        let c = peek(&reader^) |> Option.unwrap;
        &mut reader^.position |> Position.advance(c);
    );

    const read_while = (reader :: &mut Reader, f :: Char -> Bool) -> RcStr => (
        let start = reader^.position.string_encoding_index;
        while peek(&reader^) is :Some c do (
            if not f(c) then break;
            advance(reader);
        );
        let end = reader^.position.string_encoding_index;
        &reader^.contents |> RcStr.substring(start, end - start)
    );
);
