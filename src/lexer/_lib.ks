use (import "./common.ks").*;
use (import "./comment.ks").*;
use (import "./ident.ks").*;
use (import "./number.ks").*;
use (import "./punct.ks").*;
use (import "./string.ks").*;

const root_scope = @current_scope;

const read_fns :: ArrayList.t[ReadFn] = (
    let mut read_fns = ArrayList.new();
    &mut read_fns |> ArrayList.push_back(read_line_comment);
    &mut read_fns |> ArrayList.push_back(read_block_comment);
    &mut read_fns |> ArrayList.push_back(read_raw_ident);
    &mut read_fns |> ArrayList.push_back(read_raw_keyword);
    &mut read_fns |> ArrayList.push_back(read_punct);
    &mut read_fns |> ArrayList.push_back(read_ident);
    &mut read_fns |> ArrayList.push_back(read_string);
    &mut read_fns |> ArrayList.push_back(read_hex_number);
    &mut read_fns |> ArrayList.push_back(read_number);
    read_fns
);

module:

const Lexer = root_scope.Lexer;

impl Lexer as module = (
    module:

    const new = (source :: Source) -> Lexer => {
        .source,
        .reader = Reader.new(source.contents),
    };

    const next = (lexer :: &mut Lexer) -> Token.t => (
        with Context = {
            .lexer,
            .reader = &mut lexer^.reader,
            .next_token = next_impl,
        };
        next_impl()
    );

    const next_impl = () -> Token.t => (
        let ctx = @current Context;
        let reader = ctx.reader;
        with TokenStart = reader^.position;
        skip_whitespace();
        let start = reader^.position;
        let shape = with_return (
            with Diagnostic.UnwindableHandler = {
                .unwind_on_error = () -> Never => (
                    let start = start.string_encoding_index;
                    let end = reader^.position.string_encoding_index;
                    return :Error {
                        .raw = String.substring(reader^.contents, start, end - start),
                    }
                ),
            };
            if Reader.peek(&reader^) is :None then (
                return :Eof;
            );
            for &read_fn in ArrayList.iter(&read_fns) do (
                if read_fn() is :Some shape then (
                    return shape;
                );
            );
            unexpected_char("None of the read fns returned Some")
        );
        let end = reader^.position;
        let span = { .start, .end, .path = ctx.lexer^.source.path };
        { .shape, .span }
    );
);
