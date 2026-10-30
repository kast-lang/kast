use (import "./output.ks").*;
use (import "./diagnostic.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./position.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./ast.ks").*;
use (import "./lexer/_lib.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./parser.ks").*;
use (import "./rc_str.ks").*;
use std.rc.Rc;

module:

const Highlight = (
    module:

    const TokenType = newtype (
        | :StringDelimeter
        | :StringContent
        | :Escape
        | :Keyword
        | :Number
        | :Ident
        | :RawIdent
        | :Regular
        | :Error
        | :Comment
        | :SyntaxCommand
    );

    const OutputT = newtype {
        .print :: (Span, TokenType, &str) -> (),
        .replace_ast :: Option.t[type ((&Ast.t, &Ast.t -> ()) -> ())],
        .move_to :: (Position, .print_whitespace :: Bool) -> (),
    };

    const Context = @context OutputT;

    const print_single_line = (ast :: &Ast.t) => with_return (
        with Context = new_output_with(
            :Terminal,
            .start = ast^.span.start,
            .before_new_line = () => (
                ansi.with_mode(
                    :Dim,
                    () => locked_output.write_str(" ..."),
                );
                return;
            ),
        );
        walk_ast(ast);
    );

    const new_output = (mode :: OutputMode) -> OutputT => (
        new_output_with(
            mode,
            .start = Position.beginning(),
            .before_new_line = () => (),
        )
    );

    const new_output_with = (
        mode :: OutputMode,
        .start :: Position,
        .before_new_line :: () -> (),
    ) => match mode with (
        | :Terminal => (
            let mut state = Rc.new({
                .position = start,
                # Needed because of replace within replace when
                # doing structural find and replace :)
                .printed_something_after_moving = true,
            });
            let move_to = @move (target_position :: Position, .print_whitespace :: Bool) => (
                if state^.printed_something_after_moving and print_whitespace then (
                    while state^.position.line < target_position.line do (
                        before_new_line();
                        state^.position.line += 1;
                        state^.position.column = PositionColumn.zero();
                        locked_output.write_str("\n");
                    );
                    let current_column = state^.position.column.string_encoding;
                    let target_column = target_position.column.string_encoding;
                    for _ in current_column..target_column do (
                        locked_output.write_str(" ");
                    );
                );
                state^.position = target_position;
                state^.printed_something_after_moving = false;
            );
            let print_token = @move (span, token_type, s) => (
                move_to(span.start, .print_whitespace = true);
                let mode :: Option.t[ansi.Mode] = match token_type with (
                    | :Keyword => :Some :Magenta
                    | :Number => :Some :Italic
                    | :StringContent => :Some :Green
                    | :StringDelimeter => :Some :Cyan
                    | :Escape => :Some :Cyan
                    | :RawIdent => :None
                    | :Ident => :None
                    | :Regular => :None
                    | :Error => :Some :Red
                    | :Comment => :Some :Gray
                    | :SyntaxCommand => :Some :Yellow
                );
                match mode with (
                    | :Some mode => ansi.with_mode(mode, () => locked_output.write_str(s))
                    | :None => locked_output.write_str(s)
                );
                state^.position = span.end;
                state^.printed_something_after_moving = true;
            );
            {
                .print = print_token,
                .replace_ast = :None,
                .move_to,
            }
        )
    );

    const OutputMode = newtype (
        | :Terminal
        # TODO | :Html
    );

    impl OutputMode as Parse = {
        .parse = s => (
            if s == "terminal" then (
                :Terminal
            ) else if s == "html" then (
                # :Html
                Diagnostic.abort("TODO html mode")
            ) else (
                Diagnostic.abort(&format!("Unknown highlight mode \(String.escape(s))") |> as_str)
            )
        ),
    };

    const highlight = (parsed :: &Parser.Parsed, output :: OutputT) => (
        with Context = output;
        let {
            .ast = ref ast,
            .ignored_trailing_tokens = ref ignored_trailing_tokens,
            .eof,
        } = parsed^;
        walk_ast(ast);
        walk_ignored_tokens(ignored_trailing_tokens);
        output.move_to(eof, .print_whitespace = true);
    );

    const walk_string_parts = (
        content_token_type :: TokenType,
        parts :: &ArrayList.t[Token.RawStringPart],
    ) => (
        for part in parts |> ArrayList.iter do (
            match part^ with (
                | :Content { .raw = ref raw, .span } => (
                    (@current Context).print(span, content_token_type, raw |> RcStr.as_str);
                )
                | :Escape { .raw = ref raw, .span } => (
                    (@current Context).print(span, :Escape, raw |> RcStr.as_str);
                )
            );
        );
    );

    const walk_ast = (original_ast :: &Ast.t) => (
        let ctx = @current Context;
        let mut replaced = false;
        if ctx.replace_ast is :Some f then (
            f(
                original_ast,
                replace_ast => (
                    ctx.move_to(original_ast^.span.start, .print_whitespace = true);
                    ctx.move_to(replace_ast^.span.start, .print_whitespace = false);
                    walk_ast(replace_ast);
                    ctx.move_to(original_ast^.span.end, .print_whitespace = false);
                    replaced = true;
                )
            );
        );
        if not replaced then (
            walk_ast_impl(original_ast);
        );
    );

    const walk_ast_impl = (ast :: &Ast.t) => (
        let ctx = @current Context;
        walk_ignored_tokens(&ast^.ignored_tokens_before^);
        match ast^.shape^ with (
            | :Empty => ()
            | :Token ref token => (
                match token^.shape^ with (
                    | :Ident { .raw = ref raw, .string = ref string, ... } => (
                        match string^ with (
                            | :None => (
                                ctx.print(token^.span, :Ident, raw |> RcStr.as_str);
                            )
                            | :Some {
                                {
                                    .open = ref open,
                                    .close = ref close,
                                    .raw_parts = ref raw_parts,
                                    ...
                                },
                                .at_token = ref at_token,
                            } => (
                                ctx.print(at_token^.span, :Ident, Token.raw(at_token));
                                ctx.print(open^.span, :RawIdent, Token.raw(open));
                                walk_string_parts(:RawIdent, raw_parts);
                                ctx.print(close^.span, :RawIdent, Token.raw(close));
                            )
                        )
                    )
                    | :Number { .raw = ref raw, ... } => (
                        ctx.print(token^.span, :Number, raw |> RcStr.as_str);
                    )
                    | :String {
                        .open = ref open,
                        .close = ref close,
                        .raw_parts = ref raw_parts,
                        ...
                    } => (
                        ctx.print(open^.span, :StringDelimeter, Token.raw(open));
                        walk_string_parts(:StringContent, raw_parts);
                        ctx.print(close^.span, :StringDelimeter, Token.raw(close));
                    )
                )
            )
            | :InterpolatedString {
                .delimiter = _,
                .open = ref open,
                .parts = ref parts,
                .close = ref close,
                .stripped_indentation = _,
            } => (
                ctx.print(open^.span, :StringDelimeter, Token.raw(open));
                for part in parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content {
                            .raw = _,
                            .raw_parts = ref raw_parts,
                            .span = _,
                            .contents = _,
                        } => (
                            walk_string_parts(:StringContent, raw_parts);
                        )
                        | :Interpolated {
                            .open = ref open,
                            .close = ref close,
                            .ast = ref inner,
                            .ignored_trailing_tokens = ref ignored_trailing_tokens,
                        } => (
                            ctx.print(open^.span, :Escape, Token.raw(open));
                            walk_ast(inner);
                            walk_ignored_tokens(ignored_trailing_tokens);
                            ctx.print(close^.span, :Escape, Token.raw(close));
                        )
                    );
                );
                ctx.print(close^.span, :StringDelimeter, Token.raw(close));
            )
            | :Rule { .root = ref root, ... } => (
                walk_ast_group(root);
            )
            | :Syntax {
                .command = ref command,
                .value_after = ref value_after,
            } => (
                for token in &command^.raw_tokens |> ArrayList.iter do (
                    ctx.print(token^.span, :SyntaxCommand, Token.raw(token));
                );
                if value_after^ is :Some ref ast then (
                    walk_ast(ast);
                );
            )
            | :Error { .parts = ref parts } => (
                walk_ast_parts(parts)
            )
        );
    );

    const walk_ast_group = (group :: &Ast.Group) => (
        walk_ast_parts(&group^.parts);
    );

    const walk_ast_parts = (parts :: &ArrayList.t[Ast.Part]) => (
        for part in parts |> ArrayList.iter do (
            match part^ with (
                | :Keyword ref token => (
                    (@current Context).print(token^.span, :Keyword, Token.raw(token));
                )
                | :Value ref ast => (
                    walk_ast(&ast^^);
                )
                | :Group ref inner_group => (
                    walk_ast_group(&inner_group^^);
                )
                | :Ignored ref token => (
                    walk_ignored_token(token);
                )
            );
        );
    );

    const walk_ignored_token = (token :: &Token.t) => (
        if token^.shape^ is :Comment { .raw = ref raw, ... } then (
            (@current Context).print(token^.span, :Comment, raw |> RcStr.as_str);
        ) else (
            # ignored because of error
            (
                @current Context
            ).print(token^.span, :Error, Token.raw(token));
        )
    );

    const walk_ignored_tokens = (tokens :: &ArrayList.t[Token.t]) => (
        for token in tokens |> ArrayList.iter do (
            walk_ignored_token(token);
        );
    );
);
