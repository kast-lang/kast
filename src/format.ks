use (import "./tuple.ks").*;
use (import "./diagnostic.ks").*;
use (import "./output.ks").*;
use (import "./parser.ks").*;
use (import "./source.ks").*;
use (import "./source_path.ks").*;
use (import "./lexer/_lib.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./token_stream.ks").*;
use (import "./ast.ks").*;
use (import "./span.ks").*;
use (import "./position.ks").*;
use (import "./token.ks").*;
use (import "./highlight.ks").*;

module:

const Format = (
    module:

    const ContextT = newtype {
        .queued_newlines :: Int32,
        .prev_span :: Span,
        .prev_was_block_comment :: Bool,
        .prev_was_string_content :: Bool,
        .just_printed_newline :: Bool,
        .queued_indentation :: Int32,
        .indentation_width :: Int32,
    };

    const Context = @context ContextT;

    const IndentationT = newtype {
        .string :: String,
        .extra_in_string :: String,
    };

    const Indentation = @context IndentationT;

    const flush = () => (
        let output = @current Output;
        let mut ctx = @current Context;
        let mut indentation = @current Indentation;
        for _ in 0..ctx.queued_newlines do (
            output.write("\n");
            ctx.just_printed_newline = true;
        );
        ctx.queued_newlines = 0;
        while ctx.queued_indentation < 0 do (
            indentation.string = indentation.string
                |> String.substring_from(ctx.indentation_width);
            ctx.queued_indentation += 1;
        );
        while ctx.queued_indentation > 0 do (
            for _ in 0..ctx.indentation_width do (
                indentation.string += " ";
            );
            ctx.queued_indentation -= 1;
        );
    );

    const print_raw = (raw :: String) => with_return (
        if String.length(raw) == 0 then (
            return;
        );
        flush();
        let output = @current Output;
        let mut ctx = @current Context;
        if ctx.just_printed_newline then (
            output.write((@current Indentation).string);
        );
        output.write(raw);
        ctx.just_printed_newline = false;
    );

    const inc_indentation = () => (
        (@current Context).queued_indentation += 1;
    );

    const dec_indentation = () => (
        (@current Context).queued_indentation -= 1;
    );

    const queue_newline = () => (
        (@current Context).queued_newlines += 1;
    );

    const print_token = (token :: Token.t) => (
        let output = @current Output;
        let mut ctx = @current Context;
        let flush_before = () => (
            flush();
            if (
                ctx.just_printed_newline
                and token.span.start.line - ctx.prev_span.end.line > 1
                and not ctx.prev_was_string_content
            ) then (
                queue_newline();
            );
        );
        if token.shape is :Comment { .raw, .ty } then (
            if (
                not ctx.just_printed_newline
                and ctx.prev_span.end.line == token.span.start.line
            ) then (
                output.write(" ");
            ) else (
                if not ctx.just_printed_newline and ctx.queued_newlines == 0 then (
                    queue_newline();
                );
                flush_before();
            );
            if ctx.just_printed_newline then (
                output.write((@current Indentation).string);
            );
            output.write(raw);
            ctx.just_printed_newline = false;
            match ty with (
                | :Line => (
                    if ctx.queued_newlines == 0 then (
                        queue_newline();
                    );
                    flush();
                )
                | :Block => ()
            );
        ) else (
            if ctx.prev_was_block_comment and ctx.prev_span.end.line == token.span.start.line then (
                output.write(" ");
            );
            flush_before();
            if token.shape is :String {
                .open,
                .close,
                .raw_parts = ref raw_parts,
                .stripped_indentation,
                ...
            } then (
                print_raw(Token.raw(open));
                if stripped_indentation != "" then (
                    inc_indentation();
                );
                for part in raw_parts |> ArrayList.iter do (
                    let raw = match part^ with (
                        | :Escape { .raw, ... } => raw
                        | :Content { .raw, ... } => raw
                    );
                    print_raw_string_content(raw, .stripped_indentation);
                );
                if stripped_indentation != "" then (
                    dec_indentation();
                );
                print_raw(Token.raw(close));
            ) else (
                print_raw(Token.raw(token));
            );
        );
        ctx.prev_span = token.span;
        ctx.prev_was_block_comment = match token.shape with (
            | :Comment { .ty = :Block, ... } => true
            | _ => false
        );
        ctx.prev_was_string_content = false;
    );

    const print_raw_string_content = (
        raw :: String,
        .stripped_indentation :: String,
    ) => (
        let mut ctx = @current Context;
        let mut first = true;
        for line in raw |> String.split('\n') do (
            if first then (
                first = false;
            ) else (
                queue_newline();
            );
            let mut i = 0;
            while (
                i < String.length(line)
                and i < String.length(stripped_indentation)
            ) do (
                if String.at(stripped_indentation, i) == String.at(line, i) then (
                    i += 1;
                ) else (
                    break;
                );
            );
            let line = line |> String.substring_from(i);
            let mut i = 0;
            while i < String.length(line) do (
                if String.at(line, i) |> Char.is_whitespace then (
                    i += 1;
                ) else (
                    break;
                );
            );
            flush();
            if ctx.just_printed_newline then (
                (@current Indentation).extra_in_string = line
                    |> String.substring(0, i);
            );
            print_raw(line);
        );
        ctx.prev_was_string_content = true;
    );

    const format_to_string = (parsed :: &Parser.Parsed) -> String => (
        let mut result = "";
        let output = new_output(
            .write = s => (
                result += s;
            ),
            .indentation_string = "    ", # should depend on user setting
            .color = false,
        );
        format(parsed, output);
        result
    );

    const format = (parsed :: &Parser.Parsed, output :: OutputT) => (
        with Context = {
            .queued_newlines = 0,
            .queued_indentation = 0,
            .indentation_width = 4,
            .just_printed_newline = true,
            .prev_span = Span.empty(
                .position = Position.beginning(),
                .path = parsed^.ast.span.path,
            ),
            .prev_was_block_comment = false,
            .prev_was_string_content = false,
        };
        with Output = output;
        with Indentation = {
            .string = "",
            .extra_in_string = "",
        };
        let {
            .ast = ref ast,
            .ignored_trailing_tokens = ref ignored_trailing_tokens,
            .eof = _,
        } = parsed^;
        walk_ast(ast, .parent = :None);
        walk_ignored_tokens(ignored_trailing_tokens);
        queue_newline();
        flush();
    );

    const Parent = Option.t[type { .wrapped :: Bool, .priority :: SyntaxRule.Priority }];

    const walk_ast = (ast :: &Ast.t, .parent :: Parent) => (
        let {
            .ignored_tokens_before = ref ignored_tokens_before,
            .shape = ref shape,
            .span = _,
        } = ast^;
        walk_ignored_tokens(ignored_tokens_before);
        match shape^ with (
            | :Empty => ()
            | :Token token => print_token(token)
            | :InterpolatedString {
                .delimiter = _,
                .open,
                .parts,
                .close,
                .stripped_indentation,
            } => (
                with Indentation = {
                    .extra_in_string = "",
                    ...(@current Indentation),
                };
                print_token(open);
                if stripped_indentation != "" then (
                    inc_indentation();
                );
                for part in &parts |> ArrayList.iter do (
                    match part^ with (
                        | :Content { .raw, ... } => (
                            print_raw_string_content(raw, .stripped_indentation);
                        )
                        | :Interpolated {
                            .open,
                            .close,
                            .ast = ref inner,
                            .ignored_trailing_tokens = ref ignored_trailing_tokens,
                        } => (
                            with Indentation = {
                                .string = (
                                    let cur = @current Indentation;
                                    cur.string + cur.extra_in_string
                                ),
                                .extra_in_string = "",
                            };
                            let wrapped = open.span.end.line != close.span.start.line;
                            print_token(open);
                            if wrapped then (
                                queue_newline();
                                inc_indentation();
                            );
                            walk_ast(inner, .parent = :None);
                            walk_ignored_tokens(ignored_trailing_tokens);
                            if wrapped then (
                                queue_newline();
                                dec_indentation();
                            );
                            print_token(close);
                        )
                    );
                );
                if stripped_indentation != "" then (
                    dec_indentation();
                );
                print_token(close);
            )
            | :Rule { .rule = ref rule, .root = ref root } => (
                walk_ast_group(
                    &rule^.parts,
                    root,
                    rule^.wrap_mode,
                    .priority = rule^.priority,
                    .parent,
                );
            )
            | :Syntax { .command, .value_after } => (
                let mut prev_token_span :: Option.t[_] = :None;
                for &token in &command.raw_tokens |> ArrayList.iter do (
                    if prev_token_span is :Some prev_token_span then (
                        if (
                            prev_token_span.end.line != token.span.start.line
                            or prev_token_span.end.column.string_encoding != token.span.start.column.string_encoding
                        ) then (
                            print_raw(" ");
                        );
                    );
                    print_token(token);
                    prev_token_span = :Some token.span;
                );
                queue_newline();
                if value_after is :Some ref ast then (
                    walk_ast(ast, .parent = :None);
                );
            )
            | :Error { .parts } => (
                Diagnostic.abort("Refusing to format code with errors")
            )
        )
    );

    const walk_ast_group = (
        rule_parts :: &ArrayList.t[SyntaxRule.Part],
        group :: &Ast.Group,
        wrap_mode :: SyntaxRule.WrapMode,
        .priority :: SyntaxRule.Priority,
        .parent :: Parent,
    ) => (
        let group_wrapped = group^.span.start.line != group^.span.end.line;
        let wrapped = match wrap_mode with (
            | :Never => false
            | :Always => true
            | :IfAnyNonAssociative => group_wrapped
            | :IfAnyAssociative => match parent with (
                | :Some parent => group_wrapped or (parent.wrapped and parent.priority == priority)
                | :None => group_wrapped
            )
        );
        let mut unnmed_child_idx = 0;
        let next_member = name => match name with (
            | :Some name => :Name name
            | :None => (
                let result = :Index unnmed_child_idx;
                unnmed_child_idx += 1;
                result
            )
        );
        let mut part_idx = 0;
        let skip_ignored_parts = () => (
            while part_idx < &group^.parts |> ArrayList.length do (
                let part = &group^.parts |> ArrayList.at(part_idx);
                if part^ is :Ignored token then (
                    walk_ignored_token(token);
                ) else (
                    break;
                );
                part_idx += 1;
            );
        );
        let peek_part = () -> Option.t[type (&Ast.Part)] => (
            if part_idx < &group^.parts |> ArrayList.length then (
                :Some (&group^.parts |> ArrayList.at(part_idx))
            ) else (
                :None
            )
        );
        for part in rule_parts |> ArrayList.iter do (
            match part^ with (
                | :Keyword expected_keyword => with_return (
                    skip_ignored_parts();
                    let keyword_token = match peek_part() with (
                        | :Some &(:Keyword token) => token
                        | _ => panic("expected keyword token")
                    );
                    if expected_keyword != Token.raw(keyword_token) then (
                        panic("Expected different keyword");
                    );
                    print_token(keyword_token);
                )
                | :Whitespace whitespace => (
                    let s = if wrapped then (
                        whitespace.wrap
                    ) else (
                        whitespace.no_wrap
                    );
                    for c in String.iter(s) do (
                        if c == '\t' then (
                            inc_indentation();
                        ) else if c == '\n' then (
                            queue_newline();
                        ) else if c == '\\' then (
                            dec_indentation();
                        ) else if c == ' ' then (
                            print_raw(" ");
                        )
                    );
                )
                | :Value { .name, ... } => (
                    let member = next_member(name); # we use this to advance unnamed idx
                    skip_ignored_parts();
                    let child = match peek_part() with (
                        | :Some &(:Value ref child) => child
                        | _ => panic("Expected value part")
                    );
                    walk_ast(child, .parent = :Some { .wrapped, .priority });
                )
                | :Group {
                    .name,
                    .parts = ref rule_group_parts,
                    .quantifier,
                    .wrap_mode = override_wrap_mode,
                    .span = _,
                } => (
                    let member = next_member(name);
                    let child :: Option.t[_] = match &group^.children |> Tuple.get_opt(member) with (
                        | :Some &(:Group ref group) => :Some group
                        | :None => :None
                        | :Some &(:Value _) => panic("Expected group, got value")
                    );
                    match quantifier with (
                        | :None => if child is :None then (
                            panic("Expected group")
                        )
                        | :Optional => ()
                    );
                    if child is :Some group then (
                        skip_ignored_parts();
                        match peek_part() with (
                            | :Some &(:Group _) => ()
                            | _ => panic("expected group part")
                        );
                        walk_ast_group(
                            rule_group_parts,
                            group,
                            override_wrap_mode |> Option.unwrap_or(wrap_mode),
                            .priority,
                            .parent,
                        );
                    );
                )
            )
        );
        while part_idx < &group^.parts |> ArrayList.length do (
            let part = &group^.parts |> ArrayList.at(part_idx);
            if part^ is :Ignored token then (
                walk_ignored_token(token);
            ) else (
                panic("unexpected part");
            );
            part_idx += 1;
        );
    );

    const walk_ignored_tokens = (tokens :: &ArrayList.t[Token.t]) => (
        for token in tokens |> ArrayList.iter do (
            walk_ignored_token(token^);
        );
    );

    const walk_ignored_token = (token :: Token.t) => (
        print_token(token);
    );
);
