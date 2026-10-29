use (import "./common.ks").*;
use (import "./string.ks").*;

module:

const is_ident_start = (c :: Char) -> Bool => (
    c == '_' or Char.is_ascii_alpha(c)
);

const read_ident :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    let c = Reader.peek(&reader^) |> Option.unwrap_or_else(() => return :None);
    if not is_ident_start(c) then return :None;
    let start = reader^.position.string_encoding_index;
    Reader.advance(reader);
    reader |> Reader.read_while(c => Char.is_ascii_alphanumeric(c) or c == '_');
    let end = reader^.position.string_encoding_index;
    let raw = RcStr.substring(&reader^.contents, start, end - start);
    :Some :Ident {
        .raw,
        .string = :None,
        .name = raw,
    }
);

const read_raw_ident :: ReadFn = () => with_return (
    let ctx = @current Context;
    let lexer = ctx.lexer;
    let reader = ctx.reader;
    if not &reader^ |> next_is('@') then (
        return :None;
    );
    let at_token :: Token.t = {
        .shape = Rc.new(
            :Punct {
                .raw = RcStr.substring(
                    &reader^.contents,
                    reader^.position.string_encoding_index,
                    1,
                ),
            }
        ),
        .span = Span.single_char(
            .position = reader^.position,
            .char = :Some '@',
            .path = lexer^.source.path,
        ),
    };
    match &reader^ |> Reader.peek2 with (
        | :Some c => (
            if c != '"' then return :None;
        )
        | :None => return :None
    );
    let start = lexer^.reader.position;
    reader |> Reader.advance;
    let string_token = read_string() |> Option.unwrap;
    let end = lexer^.reader.position;
    let span :: Span = {
        .start,
        .end,
        .path = lexer^.source.path,
    };
    let raw = RcStr.substring(
        &reader^.contents,
        start.string_encoding_index,
        end.string_encoding_index - start.string_encoding_index,
    );
    let string = match string_token with (
        | :String string => string
        | :InterpolatedString _ => (
            error_with_span(span, "Raw idents can't use interpolated strings")
        )
    );
    :Some :Ident {
        .raw,
        .string = :Some {
            string,
            .at_token,
        },
        .name = string.contents,
    }
);
