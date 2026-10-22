use (import "./common.ks").*;

module:

const read_line_comment :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    if not next_is(&reader^, '#') then (
        return :None;
    );
    let start = reader^.position.string_encoding_index;
    reader |> Reader.read_while(c => c != '\n');
    let end = reader^.position.string_encoding_index;
    :Some :Comment {
        .raw = RcStr.substring(&reader^.contents, start, end - start),
        .ty = :Line,
    }
);

const read_block_comment :: ReadFn = () => with_return (
    let ctx = @current Context;
    let reader = ctx.reader;
    if not next_two_are(&reader^, '(', '#') then (
        return :None
    );
    let start = reader^.position.string_encoding_index;
    let mut nest_depth :: Int32 = 0;
    loop (
        if next_two_are(&reader^, '(', '#') then (
            Reader.advance(reader);
            Reader.advance(reader);
            nest_depth += 1;
            continue;
        );
        if next_two_are(&reader^, '#', ')') then (
            nest_depth -= 1;
            Reader.advance(reader);
            Reader.advance(reader);
            if nest_depth == 0 then break;
            continue;
        );
        if Reader.peek(&reader^) is :None then (
            error_token("Unclosed block comment");
        );
        Reader.advance(reader);
    );
    let end = reader^.position.string_encoding_index;
    :Some :Comment {
        .raw = RcStr.substring(&reader^.contents, start, end - start),
        .ty = :Block,
    }
);
