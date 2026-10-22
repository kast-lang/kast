use (import "./common.ks").*;
use (import "./ident.ks").*;

module:

const read_punct :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    const is_punct = (c :: Char) -> Bool => (
        if c == '_' or c == '\'' or c == '"' then (
            false
        ) else if Char.is_whitespace(c) then (
            false
        ) else if Char.is_ascii_alphanumeric(c) then (
            false
        ) else (
            true
        )
    );
    const is_single_punct = (c :: Char) -> Bool => (
        "@(){}[]&^$;\\," |> String.index_of(c) >= 0
    );
    let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
    if not is_punct(c) then return :None;
    let start = reader^.position.string_encoding_index;
    Reader.advance(reader);
    if not is_single_punct(c) then (
        reader |> Reader.read_while(c => is_punct(c) and not is_single_punct(c));
    );
    let end = reader^.position.string_encoding_index;
    :Some :Punct {
        .raw = String.from_str(String.substring(&reader^.contents^ |> as_str, start, end - start)),
    }
);

const read_raw_keyword :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    if not &reader^ |> next_is('@') then (
        return :None;
    );
    let start = reader^.position.string_encoding_index;
    reader |> Reader.advance;
    let _ = read_ident();
    let end = reader^.position.string_encoding_index;
    :Some :Punct {
        .raw = String.from_str(String.substring(&reader^.contents^ |> as_str, start, end - start)),
    }
);
