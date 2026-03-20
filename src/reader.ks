use (import "./position.ks").*;

module:

const Reader = newtype {
    .contents :: String,
    .position :: Position,
};

impl Reader as module = (
    module:
    
    const new = (contents :: String) -> Reader => {
        .contents,
        .position = Position.beginning(),
    };
    
    const prev = (reader :: &Reader) -> Option.t[Char] => (
        if reader^.position.index == 0 then (
            :None
        ) else (
            :Some (reader^.contents |> String.at(reader^.position.index - 1))
        )
    );
    
    const peek = (reader :: &Reader) -> Option.t[Char] => (
        if reader^.position.index < reader^.contents |> String.length then (
            :Some (reader^.contents |> String.at(reader^.position.index))
        ) else (
            :None
        )
    );
    
    const peek2 = (reader :: &Reader) -> Option.t[Char] => (
        match peek(reader) with (
            | :Some peek => (
                let next_index = reader^.position.index + Char.string_encoding_len(peek);
                if next_index < reader^.contents |> String.length then (
                    :Some (reader^.contents |> String.at(next_index))
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
    
    const read_while = (reader :: &mut Reader, f :: Char -> Bool) -> String => (
        let start = reader^.position.index;
        while peek(&reader^) is :Some c do (
            if not f(c) then break;
            advance(reader);
        );
        let end = reader^.position.index;
        reader^.contents |> String.substring(start, end - start)
    );
);
