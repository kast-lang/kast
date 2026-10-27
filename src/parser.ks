use (import "./output.ks").*;
use (import "./tuple.ks").*;
use (import "./log.ks").*;
use (import "./diagnostic.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./position.ks").*;
use (import "./source_path.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "./rc_str.ks").*;
use (import "../deps/uri/src/lib.ks").*;
use std.rc.Rc;
use std.collections.OrdMap;
use std.collections.OrdSet;

@syntax "is" 41 @wrap never = value " " "is" " " pattern;
impl syntax (value is pattern) = `(
    match $value with (
        | $pattern => true
        | _ => false
    )
);

const iter_find = [T] (
    iterable :: std.iter.Iterable[T],
    predicate :: T -> Bool,
) -> Option.t[T] => with_return (
    for value in iterable do (
        if predicate(value) then return :Some value;
    );
    :None
);

const internal_error = [T] (span :: Span, message :: () -> ()) -> T => (
    let diagnostic = {
        .severity = :Error,
        .source = :Internal,
        .span,
        .message,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

module:

const Parser = (
    module:

    const StaticContextT = newtype {
        .token_stream :: &mut TokenStream.t,
        .entire_source_span :: Span,
        .path :: SourcePath,
        .ignored_tokens :: ArrayList.t[Token.t],
    };
    const DynamicContextT = newtype {
        .ruleset :: &SyntaxRuleset.t,
        .continuation_keywords :: OrdSet.t[RcStr],
    };
    const StaticContext = @context type (&mut StaticContextT);
    const DynamicContext = @context type (&mut DynamicContextT);

    const ParsingRuleCtx = @context type {
        .print :: () -> (),
    };

    const ParseResult = newtype (
        | :MadeProgress Ast.t
        | :NoProgress
    );

    const ParseExtendResult = newtype (
        | :MadeProgress Ast.t
        | :NoProgress {
            .starting_value :: Option.t[Ast.t],
        }
    );

    const try_parse_or_extend_minimal = (
        starting_value :: Option.t[Ast.t],
        .priority_filter :: SyntaxRule.PriorityFilter,
    ) -> ParseExtendResult => with_return (
        let ctx = @current StaticContext;
        let mut dyn_ctx = @current DynamicContext;
        skip_comments_and_errors();
        if starting_value is :None then (
            let peek = &ctx^.token_stream^ |> TokenStream.peek;
            let peek_raw = &peek.shape^ |> Token.Shape.raw;
            if dyn_ctx^.ruleset^.syntax_keyword is :Some ref syntax_keyword then (
                if peek_raw == syntax_keyword |> RcStr.as_str then (
                    let syntax_token = peek;
                    let command_raw_tokens = ctx^.token_stream
                        |> TokenStream.start_recording;
                    let recording = ctx^.token_stream
                        |> TokenStream.start_recording;
                    let ignored_tokens_before = Rc.new(claim_ignored_tokens());
                    ctx^.token_stream |> TokenStream.advance;
                    with Diagnostic.UnwindableHandler = {
                        .unwind_on_error = () -> Never => (
                            let raw_tokens = ctx^.token_stream
                                |> TokenStream.finish_recording(recording);
                            let mut parts = ArrayList.new();
                            let mut span = { ...syntax_token.span };
                            for token in raw_tokens |> ArrayList.into_iter do (
                                &mut parts |> ArrayList.push_back(:Ignored token);
                                span.end = token.span.end;
                            );
                            return :MadeProgress {
                                .ignored_tokens_before,
                                .shape = Rc.new(:Error { .parts }),
                                .span,
                            }
                        ),
                    };
                    let {
                        .command,
                        .raw_tokens = _,
                    } = SyntaxParser.parse_syntax_command(ctx^.token_stream);
                    let command_raw_tokens = ctx^.token_stream
                        |> TokenStream.finish_recording(command_raw_tokens);
                    let { command, new_ruleset } = match command with (
                        | :FromScratch => { :FromScratch, SyntaxRuleset.new() }
                        | :Rule rule => (
                            let rule = Rc.new(rule);
                            let mut new_ruleset = SyntaxRuleset.clone(dyn_ctx^.ruleset);
                            &mut new_ruleset |> SyntaxRuleset.add(rule);
                            { :Rule rule, new_ruleset }
                        )
                    );
                    let mut new_dyn_ctx = {
                        .ruleset = &new_ruleset,
                        .continuation_keywords = OrdSet.clone(&dyn_ctx^.continuation_keywords),
                    };
                    with DynamicContext = &mut new_dyn_ctx;
                    let value_after = match try_parse(.priority_filter) with (
                        | :MadeProgress ast => :Some ast
                        | :NoProgress => :None
                    );
                    return :MadeProgress {
                        .shape = Rc.new(:Syntax {
                            .command = {
                                .shape = command,
                                .raw_tokens = command_raw_tokens,
                            },
                            .value_after,
                        }),
                        .ignored_tokens_before,
                        .span = {
                            .start = syntax_token.span.start,
                            .end = (
                                let prev_token = &ctx^.token_stream^
                                    |> TokenStream.prev
                                    |> Option.unwrap;
                                prev_token.span.end
                            ),
                            .path = syntax_token.span.path,
                        }
                    }
                );
            );
            if try_parse_single_token() is :MadeProgress ast then (
                return :MadeProgress ast;
            );
        );
        try_parse_single_rule_node(starting_value, .priority_filter)
    );

    const skip_comments_and_errors = () => (
        let mut ctx = @current StaticContext;
        loop (
            let peek = &ctx^.token_stream^ |> TokenStream.peek;
            match peek.shape^ with (
                | :Comment _ => ()
                | :Error _ => ()
                | _ => break
            );
            Log.debug(
                () => (
                    write_str("Skipping ");
                    Token.print(&peek);
                )
            );
            &mut ctx^.ignored_tokens |> ArrayList.push_back(peek);
            ctx^.token_stream |> TokenStream.advance;
        );
    );

    const claim_ignored_tokens = () -> ArrayList.t[Token.t] => (
        let mut ctx = @current StaticContext;
        let result = ctx^.ignored_tokens;
        ctx^.ignored_tokens = ArrayList.new();
        result
    );

    const try_parse_single_token = () -> ParseResult => (
        let ctx = @current StaticContext;
        let dyn_ctx = @current DynamicContext;
        let peek = &ctx^.token_stream^ |> TokenStream.peek;
        let peek_raw = &peek |> Token.raw;
        let do_parse = match peek.shape^ with (
            | :Comment _ => panic("unreachable")
            | :Punct _ => false
            | :Ident { .raw = ref raw, ... } => not (&dyn_ctx^.ruleset^.keywords |> OrdSet.contains(raw))
            | :String _ => true
            | :InterpolatedString _ => true
            | :Number _ => true
            | :Eof => false
            | :Error _ => panic("unreachable")
        );
        if do_parse then (
            let ignored_tokens_before = Rc.new(claim_ignored_tokens());
            Log.debug_msg(&format!("Parsed single token \(String.escape(peek_raw))") |> as_str);
            ctx^.token_stream |> TokenStream.advance;
            let parsed = match peek.shape^ with (
                | :InterpolatedString {
                    .delimiter,
                    .open,
                    .close,
                    .parts = ref token_parts,
                    .raw = _,
                    .stripped_indentation,
                } => (
                    let mut ast_parts = ArrayList.new();
                    for token_part in token_parts |> ArrayList.iter do (
                        let ast_part :: Ast.InterpolatedStringPart = match token_part^ with (
                            | :Content part => :Content part
                            | :Interpolated {
                                .open,
                                .close,
                                .tokens = ref tokens,
                                .span,
                            } => :Interpolated (
                                let mut token_stream = (
                                    let mut i = 0;
                                    TokenStream.from_fn(
                                        () => (
                                            if i < tokens |> ArrayList.length then (
                                                let token = (tokens |> ArrayList.at(i))^;
                                                i += 1;
                                                token
                                            ) else (
                                                {
                                                    .shape = Rc.new(:Eof),
                                                    .span = {
                                                        .start = span.end,
                                                        .end = span.end,
                                                        .path = span.path,
                                                    },
                                                }
                                            )
                                        )
                                    )
                                );
                                let parsed = parse_with(
                                    .ruleset = dyn_ctx^.ruleset,
                                    .token_stream = &mut token_stream,
                                    .entire_source_span = span,
                                    .path = span.path,
                                    .eof_name = ")",
                                );
                                {
                                    .open,
                                    .close,
                                    .ast = parsed.ast,
                                    .ignored_trailing_tokens = parsed.ignored_trailing_tokens,
                                }
                            )
                        );
                        &mut ast_parts |> ArrayList.push_back(ast_part);
                    );
                    :InterpolatedString {
                        .delimiter,
                        .open,
                        .close,
                        .parts = ast_parts,
                        .stripped_indentation,
                    }
                )
                | _ => :Token peek
            );
            :MadeProgress {
                .shape = Rc.new(parsed),
                .ignored_tokens_before,
                .span = peek.span,
            }
        ) else (
            :NoProgress
        )
    );

    const ParsedPart = newtype (
        | :Ignored Token.t
        | :Keyword Token.t
        | :Value Ast.t
    );

    impl ParsedPart as module = (
        module:

        const span = (part :: &ParsedPart) -> Span => match part^ with (
            | :Ignored token => token.span
            | :Keyword token => token.span
            | :Value ast => ast.span
        );
    );

    const try_parse_single_rule_node = (
        starting_value :: Option.t[Ast.t],
        .priority_filter :: SyntaxRule.PriorityFilter,
    ) -> ParseExtendResult => with_return (
        let ctx = @current StaticContext;
        let dyn_ctx = @current DynamicContext;
        let mut node :: &SyntaxRuleset.Node = match starting_value with (
            | :None => &dyn_ctx^.ruleset^.root
            | :Some _ => (
                match &dyn_ctx^.ruleset^.root.next |> OrdMap.get(&(:Value)) with (
                    | :Some edge => &edge^.target
                    | :None => return :NoProgress { .starting_value }
                )
            )
        );

        let mut made_progress = false;
        let mut parts :: ArrayList.t[ParsedPart] = ArrayList.new();
        if starting_value is :Some value then (
            &mut parts |> ArrayList.push_back(:Value value);
        );

        let parent_parsing_rule_context = @current ParsingRuleCtx;
        with ParsingRuleCtx = {
            .print = () => (
                let output = @current Output;
                write_str("Parsing");
                for part in &parts |> ArrayList.iter do (
                    write_str(" ");
                    match part^ with (
                        | :Keyword ref token => write_str(token |> Token.raw)
                        | :Value ref ast => (
                            write_str("_ at ");
                            Span.print(&ast^.span);
                        )
                    );
                );
                write_str("\nwhile ");
                parent_parsing_rule_context.print();
            ),
        };

        loop (
            skip_comments_and_errors();

            let peek = &ctx^.token_stream^ |> TokenStream.peek;
            let peek_raw = &peek |> Token.raw;

            Log.debug(
                () => (
                    let output = @current Output;
                    write_str("Looking at ");
                    Token.print(&peek);
                    write_str("\ncontinuation keywords:");
                    for keyword in &dyn_ctx^.continuation_keywords |> OrdSet.iter do (
                        write_str(" ");
                        write_escaped(keyword);
                    );
                    output.write("\n");
                    (@current ParsingRuleCtx).print();
                )
            );

            let should_follow_edge = (edge :: &SyntaxRuleset.Edge) -> Bool => with_return (
                if edge^.key is :Keyword ref keyword then (
                    if (
                        not made_progress
                        and &dyn_ctx^.continuation_keywords
                            |> OrdSet.contains(keyword)
                    ) then (
                        Log.debug_msg(&format!("Not following keyword \(String.escape(keyword |> RcStr.as_str)) because it is a continuation keyword") |> as_str);
                        return false;
                    );
                );
                SyntaxRule.priority_matches(edge^.max_rule_priority, priority_filter)
            );
            # Try to follow with keyword
            let keyword_edge = :Keyword RcStr.from_String(String.from_str(peek_raw)); # TODO no alloc
            if &node^.next |> OrdMap.get(&keyword_edge) is :Some edge then (
                if should_follow_edge(edge) then (
                    for token in claim_ignored_tokens() |> ArrayList.into_iter do (
                        &mut parts |> ArrayList.push_back(:Ignored token);
                    );
                    ctx^.token_stream |> TokenStream.advance;
                    Log.debug_msg(&format!("Following with keyword \(String.escape(peek_raw))") |> as_str);
                    node = &edge^.target;
                    made_progress = true;
                    &mut parts |> ArrayList.push_back(:Keyword peek);
                    continue;
                );
            );
            # If no progress was made,
            # then we can start with a simple token value
            # Otherwise this would be infinite recursion
            if &parts |> ArrayList.length != 0 then (
                # Try to follow with value
                if &node^.next |> OrdMap.get(&(:Value)) is :Some edge then (
                    if should_follow_edge(edge) then (
                        let value = (
                            let mut new_dyn_ctx = {
                                .ruleset = dyn_ctx^.ruleset,
                                .continuation_keywords = (
                                    let mut keywords = if edge^.max_value_priority is :Any then (
                                        OrdSet.new()
                                    ) else (
                                        OrdSet.clone(&dyn_ctx^.continuation_keywords)
                                    );
                                    for next_edge in &edge^.target.next |> OrdMap.iter do (
                                        if next_edge^.key is :Keyword keyword then (
                                            &mut keywords |> OrdSet.add(keyword);
                                        );
                                    );
                                    keywords
                                ),
                            };
                            with DynamicContext = &mut new_dyn_ctx;
                            try_parse(
                                .priority_filter = edge^.max_value_priority,
                            )
                        );
                        let value :: Option.t[_] = if value is :MadeProgress ast then (
                            :Some ast
                        ) else if edge^.max_value_priority is :Any then (
                            :Some {
                                .ignored_tokens_before = Rc.new(claim_ignored_tokens()),
                                .shape = Rc.new(:Empty),
                                .span = peek.span,
                            }
                        ) else (
                            :None
                        );
                        if value is :Some ast then (
                            Log.debug_msg("Following with value");
                            node = &edge^.target;
                            made_progress = true;
                            &mut parts |> ArrayList.push_back(:Value ast);
                            continue;
                        );
                    );
                );
            );

            break;
        );

        if not made_progress then (
            return :NoProgress { .starting_value };
        );

        let span = (
            let first_span = &parts.[0] |> ParsedPart.span;
            let last_index = (&parts |> ArrayList.length) - 1;
            let last_span = &parts.[last_index] |> ParsedPart.span;
            {
                .start = first_span.start,
                .end = last_span.end,
                .path = first_span.path,
            }
        );
        let shape = with_return (
            with Diagnostic.UnwindableHandler = {
                .unwind_on_error = () -> Never => (
                    let mut error_parts = ArrayList.new();
                    for part in parts |> ArrayList.into_iter do (
                        let part = match part with (
                            | :Ignored token => :Ignored token
                            | :Keyword token => :Keyword token
                            | :Value ast => :Value Rc.new(ast)
                        );
                        &mut error_parts |> ArrayList.push_back(part);
                    );
                    return :Error { .parts = error_parts }
                ),
            };
            let rule = node^.terminal
                |> Option.unwrap_or_else(
                    () => (
                        let diagnostic = {
                            .severity = :Error,
                            .source = :Parser,
                            .span,
                            .message = () => (
                                write_str("Can't finish parsing");
                                for part in &parts |> ArrayList.iter do (
                                    match part^ with (
                                        | :Ignored _ => ()
                                        | :Keyword token => (
                                            write_str(" ");
                                            write_str(&token |> Token.raw);
                                        )
                                        | :Value _ => write_str(" _")
                                    );
                                );
                            ),
                            .related = (
                                let mut related = ArrayList.new();
                                let peek = &ctx^.token_stream^ |> TokenStream.peek;
                                let unexpected_token = {
                                    .span = peek.span,
                                    .message = () => (
                                        write_str("Unexpected token ");
                                        Token.Shape.print(&peek.shape^);
                                    )
                                };
                                &mut related |> ArrayList.push_back(unexpected_token);
                                related
                            ),
                        };
                        Diagnostic.report_and_unwind(diagnostic)
                    )
                );
            :Rule {
                .rule,
                .root = collect_values(
                    parts,
                    &rule^,
                    .error_span = span,
                ),
            }
        );
        let ast :: Ast.t = {
            .ignored_tokens_before = Rc.new(ArrayList.new()),
            .shape = Rc.new(shape),
            .span,
        };
        Log.debug(
            () => (
                write_str("Parsed at ");
                Span.print(&span);
                write_str("\n");
                Ast.print(&ast);
            )
        );
        :MadeProgress ast
    );

    const print_parsed_parts = (parts :: &ArrayList.t[ParsedPart]) => (
        for part in parts |> ArrayList.iter do (
            write_str("- ");
            match part^ with (
                | :Ignored token => (
                    ansi.with_mode(
                        :Dim,
                        () => write_str("<ignored> "),
                    );
                    write_str(&token |> Token.raw)
                )
                | :Keyword token => write_str(&token |> Token.raw)
                | :Value ref ast => Ast.print(ast)
            );
            write_str("\n");
        );
    );

    const collect_values = (
        parsed_parts :: ArrayList.t[ParsedPart],
        rule :: &SyntaxRule.t,
        .error_span :: Span,
    ) -> Ast.Group => (
        Log.debug_msg(&format!("Collecting values for \(String.escape(&rule^.name |> RcStr.as_str))") |> as_str);
        let mut parsed_part_idx = 0;
        let result = collect_values_from(
            &mut parsed_part_idx,
            &parsed_parts,
            &rule^.parts,
            .rule_group = :None,
            .error_span,
        );
        if parsed_part_idx < &parsed_parts |> ArrayList.length then (
            print_parsed_parts(&parsed_parts);
            panic_fmt!("Too many values supplied for the rule \(String.escape(&rule^.name |> RcStr.as_str))")
        ) else (
            Log.debug_msg(&format!("Values have been collected for \(String.escape(&rule^.name |> RcStr.as_str))") |> as_str);
            result
        )
    );

    const collect_values_from = (
        parsed_part_idx :: &mut Int32,
        parsed_parts :: &ArrayList.t[ParsedPart],
        rule_group_parts :: &ArrayList.t[SyntaxRule.Part],
        .rule_group :: Option.t[type (&SyntaxRule.Group)],
        .error_span :: Span,
    ) -> Ast.Group => (
        let mut rule_part_idx = 0;
        let skip_rule_whitespace = () -> Bool => (
            let start_idx = rule_part_idx;
            while rule_part_idx < rule_group_parts |> ArrayList.length do (
                match rule_group_parts^.[rule_part_idx] with (
                    | :Whitespace _ => ()
                    | _ => break
                );
                rule_part_idx += 1;
            );
            rule_part_idx != start_idx
        );
        let mut ast_parts = ArrayList.new();
        let mut children = Tuple.new();
        while (
            parsed_part_idx^ < parsed_parts |> ArrayList.length
            and rule_part_idx < rule_group_parts |> ArrayList.length
        ) do (
            let parsed_part = &parsed_parts^.[parsed_part_idx^];
            if parsed_part^ is :Ignored token then (
                &mut ast_parts |> ArrayList.push_back(:Ignored token);
                parsed_part_idx^ += 1;
                continue;
            );
            if skip_rule_whitespace() then continue;
            let rule_part = &rule_group_parts^.[rule_part_idx];
            match ({ parsed_part, rule_part } :: { _, _ }) with (
                | { &(:Value ast), &(:Value { .name, ... }) } => (
                    Log.debug(
                        () => (
                            let output = @current Output;
                            output.write("Matched value ");
                            Span.print(&ast.span);
                        )
                    );
                    let ast = Rc.new(ast);
                    &mut ast_parts |> ArrayList.push_back(:Value ast);
                    &mut children |> Tuple.add(name, :Value ast);
                    parsed_part_idx^ += 1;
                    rule_part_idx += 1;
                )
                | { &(:Keyword ref token), &(:Keyword ref keyword) } => (
                    if token |> Token.raw != keyword |> RcStr.as_str then (
                        internal_error(
                            error_span,
                            () => (@current Output).write("Incorrectly matched keyword")
                        );
                    );
                    Log.debug(
                        () => (
                            let output = @current Output;
                            output.write("Matched keyword ");
                            Token.print(token);
                        )
                    );
                    &mut ast_parts |> ArrayList.push_back(:Keyword token^);
                    parsed_part_idx^ += 1;
                    rule_part_idx += 1;
                )
                | { _, &(:Group ref rule_group) } => (
                    let {
                        .name,
                        .parts = ref rule_group_parts,
                        .quantifier,
                        .wrap_mode = _,
                        .span = _,
                    } = rule_group^;
                    match quantifier with (
                        | :None => ()
                        | :Optional => (
                            let first_non_whitespace_part = rule_group_parts
                                |> ArrayList.iter
                                |> iter_find(part => not (part^ is :Whitespace _));
                            let expected_keyword :: &str = match first_non_whitespace_part with (
                                | :Some &(:Keyword ref keyword) => keyword |> RcStr.as_str
                                | _ => (
                                    panic("Optional group is supposed to have keyword as first part")
                                )
                            );
                            let should_match_group = match parsed_part^ with (
                                | :Keyword ref token => (
                                    token |> Token.raw == expected_keyword
                                )
                                | _ => false
                            );
                            if not should_match_group then (
                                rule_part_idx += 1;
                                continue;
                            );
                        )
                    );
                    Log.debug(
                        () => (
                            let output = @current Output;
                            output.write("Matching group at ");
                            Span.print(&rule_group^.span);
                        )
                    );
                    let group = collect_values_from(
                        parsed_part_idx,
                        parsed_parts,
                        rule_group_parts,
                        .rule_group = :Some rule_group,
                        .error_span,
                    );
                    Log.debug_msg("Group matched");
                    let group = Rc.new(group);
                    &mut ast_parts |> ArrayList.push_back(:Group group);
                    &mut children |> Tuple.add(name, :Group group);
                    rule_part_idx += 1;
                )
                | _ => (
                    # dbg.print(parsed_part^);
                    dbg.print({ .rule_part });
                    print_parsed_parts(parsed_parts);
                    internal_error(
                        error_span,
                        () => (
                            let output = @current Output;
                            write_str("mismatch matching parsed part #");
                            write_str(&to_string(parsed_part_idx^) |> as_str);
                            write_str(" and rule_group_part #");
                            write_str(&to_string(rule_part_idx) |> as_str);
                        ),
                    );
                )
            );
        );
        while rule_part_idx < rule_group_parts |> ArrayList.length do (
            let rule_part = &rule_group_parts^.[rule_part_idx];
            let fail = (type_name :: &str, name :: Option.t[RcStr], span :: Span) => internal_error(
                error_span,
                () => (
                    write_str("Rule parts were not fully matched - didn't match ");
                    write_str(type_name);
                    if name is :Some ref name then (
                        write_str(" ");
                        write(name);
                    );
                    write_str(" at ");
                    Span.print(&span);
                ),
            );
            # TODO rule_part.span
            let fail_span = {
                .start = Position.beginning(),
                .end = Position.beginning(),
                .path = :Special RcStr.from_String(String.from_str("<TODO>")),
            };
            match rule_part^ with (
                | :Whitespace _ => ()
                | :Group ref group => (
                    match group^.quantifier with (
                        | :None => fail("group", group^.name, fail_span)
                        | :Optional => ()
                    );
                )
                | :Value ref value => fail("value", value^.name, fail_span)
                | :Keyword keyword => fail("keyword", :Some keyword, fail_span)
            );
            rule_part_idx += 1;
        );
        let span = (
            let parts_len = &ast_parts |> ArrayList.length;
            if parts_len == 0 then (
                internal_error(
                    error_span,
                    () => (
                        let output = @current Output;
                        output.write("Collected zero parts?");
                    ),
                );
            );
            let first_part = &ast_parts.[0];
            let last_part = &ast_parts.[parts_len - 1];
            {
                .start = (first_part |> Ast.part_span).start,
                .end = (last_part |> Ast.part_span).end,
                .path = (first_part |> Ast.part_span).path,
            }
        );
        { .parts = ast_parts, .children, .span }
    );

    const try_parse = (.priority_filter :: SyntaxRule.PriorityFilter) -> ParseResult => (
        let mut progress = :None;
        Log.debug_msg("Try parse");
        loop (
            match try_parse_or_extend_minimal(progress, .priority_filter) with (
                | :MadeProgress ast => (
                    Log.debug_msg("Made progress");
                    progress = :Some ast;
                )
                | :NoProgress { .starting_value } => (
                    progress = starting_value;
                    break;
                )
            );
        );
        Log.debug_msg("Try parse done");
        match progress with (
            | :None => :NoProgress
            | :Some ast => :MadeProgress ast
        )
    );

    const Parsed = newtype {
        .ast :: Ast.t,
        .ignored_trailing_tokens :: ArrayList.t[Token.t],
        .eof :: Position,
    };

    const parse = (
        .ruleset :: &SyntaxRuleset.t,
        .token_stream :: &mut TokenStream.t,
        .entire_source_span :: Span,
        .path :: SourcePath,
    ) -> Parsed => (
        parse_with(
            .ruleset,
            .token_stream,
            .entire_source_span,
            .path,
            .eof_name = "EOF",
        )
    );

    const parse_with = (
        .ruleset :: &SyntaxRuleset.t,
        .token_stream :: &mut TokenStream.t,
        .entire_source_span :: Span,
        .path :: SourcePath,
        .eof_name :: &str,
    ) -> Parsed => (
        let mut ctx :: StaticContextT = {
            .token_stream,
            .entire_source_span,
            .path,
            .ignored_tokens = ArrayList.new(),
        };
        let ctx = &mut ctx;
        with StaticContext = ctx;
        let mut dyn_ctx :: DynamicContextT = {
            .ruleset,
            .continuation_keywords = OrdSet.new(),
        };
        with DynamicContext = &mut dyn_ctx;
        with ParsingRuleCtx = { .print = () => () };
        let ast = match try_parse(.priority_filter = :Any) with (
            | :MadeProgress ast => ast
            | :NoProgress => {
                .ignored_tokens_before = Rc.new(claim_ignored_tokens()),
                .shape = Rc.new(:Empty),
                .span = ctx^.entire_source_span,
            }
        );
        let mut reported_expected_eof = false;
        let eof = unwindable eof @loop (
            let peek = &ctx^.token_stream^ |> TokenStream.peek;
            if peek.shape^ is :Eof then (
                unwind eof peek.span.start;
            );
            if not reported_expected_eof then (
                let diagnostic = {
                    .severity = :Error,
                    .source = :Parser,
                    .span = peek.span,
                    .message = () => (
                        let output = @current Output;
                        output.write("Expected ");
                        output.write(eof_name);
                        output.write(", got ");
                        Token.Shape.print_impl(&peek.shape^, .verbose = false);
                    ),
                    .related = ArrayList.new(),
                };
                Diagnostic.report(diagnostic);
                reported_expected_eof = true;
            );
            &mut ctx^.ignored_tokens |> ArrayList.push_back(peek);
            ctx^.token_stream |> TokenStream.advance;
        );
        {
            .ast,
            .ignored_trailing_tokens = claim_ignored_tokens(),
            .eof,
        }
    );
);
