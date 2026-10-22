use (import "./common.ks").*;

module:

const read_hex_number :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    let start = reader^.position.string_encoding_index;
    if not next_two_are(&reader^, '0', 'x') then (
        return :None;
    );
    Reader.advance(reader);
    Reader.advance(reader);
    reader |> Reader.read_while(is_hex_digit);
    let end = reader^.position.string_encoding_index;
    :Some :Number {
        .raw = String.from_str(String.substring(&reader^.contents^ |> as_str, start, end - start)),
    }
);

const read_number :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    let start = reader^.position.string_encoding_index;
    let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
    if not Char.is_ascii_digit(c) then return :None;
    let seen_dot = match Reader.prev(&reader^) with (
        | :Some c => c == '.'
        | :None => false
    );
    Reader.read_while(reader, Char.is_ascii_digit);
    match Reader.peek(&reader^) with (
        | :Some c => (
            let digit_after_dot = match Reader.peek2(&reader^) with (
                | :Some c => Char.is_ascii_digit(c)
                | :None => false
            );
            if not seen_dot and c == '.' and digit_after_dot then (
                Reader.advance(reader);
                Reader.read_while(reader, Char.is_ascii_digit);
            )
        )
        | :None => ()
    );
    let end = reader^.position.string_encoding_index;
    :Some :Number {
        .raw = String.from_str(String.substring(&reader^.contents^ |> as_str, start, end - start)),
    }
);
