use (import "./common.ks").*;
use (import "./output.ks").*;
use (import "./tuple.ks").*;
use (import "./log.ks").*;
use (import "./error.ks").*;
use (import "./token_stream.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_ruleset.ks").*;
use (import "./ast.ks").*;
use (import "./span.ks").*;
use (import "./token.ks").*;
use (import "../deps/uri/src/lib.ks").*;
use std.collections.OrdMap;

@syntax "is" 41 @wrap never = value " " "is" " " pattern;
impl syntax (value is pattern) = `(
    match $value with (
        | $pattern => true
        | _ => false
    )
);

@syntax "index_array" 70 @wrap never = <- array "." "[" index:any "]";
impl syntax (array.[index]) = `(
    &$array |> ArrayList.at($index)
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

module:

const Parser = (
    module:
    
    const ContextT = newtype {
        .ruleset :: SyntaxRuleset.t,
        .token_stream :: &mut TokenStream.t,
        .entire_source_span :: Span,
        .uri :: Uri,
    };
    const Context = @context ContextT;
    
    const ParseResult = newtype (
        | :MadeProgress Ast.t
        | :NoProgress
    );
    
    const try_parse_or_extend_minimal = (
        starting_value :: Option.t[Ast.t],
        .priority_filter :: SyntaxRule.PriorityFilter,
    ) -> ParseResult => with_return (
        skip_comments_and_errors();
        if starting_value is :None then (
            if try_parse_single_token() is :MadeProgress ast then (
                return :MadeProgress ast;
            );
        );
        try_parse_single_rule_node(starting_value, .priority_filter)
    );
    
    const skip_comments_and_errors = () => (
        let ctx = @current Context;
        loop (
            let peek = &ctx.token_stream^ |> TokenStream.peek;
            match peek.shape with (
                | :Comment _ => ()
                | :Error _ => ()
                | _ => break
            );
            ctx.token_stream |> TokenStream.advance;
        );
    );
    
    const try_parse_single_token = () -> ParseResult => (
        let ctx = @current Context;
        let peek = &ctx.token_stream^ |> TokenStream.peek;
        let peek_raw = peek.shape |> Token.Shape.raw;
        let do_parse = match peek.shape with (
            | :Comment _ => panic("unreachable")
            | :Punct _ => false
            | :Ident { .raw, ... } => not (&ctx.ruleset.keywords |> OrdSet.contains(raw))
            | :String _ => true
            | :Number _ => true
            | :Eof => false
            | :Error _ => panic("unreachable")
        );
        if do_parse then (
            Log.debug_msg("Parsed single token " + escape_string(peek_raw));
            ctx.token_stream |> TokenStream.advance;
            :MadeProgress {
                .shape = :Token peek,
                .span = peek.span,
            }
        ) else (
            :NoProgress
        )
    );
    
    const ParsedPart = newtype (
        | :Keyword Token.t
        | :Value Ast.t
    );
    
    impl ParsedPart as module = (
        module:
        
        const span = (part :: &ParsedPart) -> Span => match part^ with (
            | :Keyword token => token.span
            | :Value ast => ast.span
        );
    );
    
    const try_parse_single_rule_node = (
        starting_value :: Option.t[Ast.t],
        .priority_filter :: SyntaxRule.PriorityFilter,
    ) -> ParseResult => with_return (
        let ctx = @current Context;
        let mut node :: &SyntaxRuleset.Node = match starting_value with (
            | :None => &ctx.ruleset.root
            | :Some _ => (
                match &ctx.ruleset.root.next |> OrdMap.get(:Value) with (
                    | :Some edge => &edge^.target
                    | :None => return :NoProgress
                )
            )
        );
        
        let mut made_progress = false;
        let mut parts :: ArrayList.t[ParsedPart] = ArrayList.new();
        if starting_value is :Some value then (
            &mut parts |> ArrayList.push_back(:Value value);
        );
        loop (
            skip_comments_and_errors();
            
            let peek = &ctx.token_stream^ |> TokenStream.peek;
            let peek_raw = peek.shape |> Token.Shape.raw;
            
            let should_follow_edge = (edge :: &SyntaxRuleset.Edge) -> Bool => (
                SyntaxRule.priority_matches(edge^.max_rule_priority, priority_filter)
            );
            
            # Try to follow with keyword
            if &node^.next |> OrdMap.get(:Keyword peek_raw) is :Some edge then (
                if should_follow_edge(edge) then (
                    ctx.token_stream |> TokenStream.advance;
                    Log.debug_msg("Following with keyword " + escape_string(peek_raw));
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
                if &node^.next |> OrdMap.get(:Value) is :Some edge then (
                    if should_follow_edge(edge) then (
                        let value = try_parse(
                            .priority_filter = edge^.max_value_priority,
                        );
                        let value = if value is :MadeProgress ast then (
                            :Some ast
                        ) else if edge^.max_value_priority is :Any then (
                            :Some {
                                .shape = :Empty,
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
            return :NoProgress;
        );
        
        let rule = node^.terminal
            |> Option.unwrap_or_else(
                () => (
                    let output = @current Output;
                    Error.report_and_unwind(
                        (&ctx.token_stream^ |> TokenStream.peek).span,
                        () => (
                            let output = @current Output;
                            output.write("Can't finish parsing");
                            for part in &parts |> ArrayList.iter do (
                                output.write(" ");
                                match part^ with (
                                    | :Keyword token => output.write(token.shape |> Token.Shape.raw)
                                    | :Value _ => output.write("_")
                                );
                            );
                        )
                    )
                )
            );
        
        let span = (
            let first_span = parts.[0] |> ParsedPart.span;
            let last_index = (&parts |> ArrayList.length) - 1;
            let last_span = parts.[last_index] |> ParsedPart.span;
            {
                .start = first_span.start,
                .end = last_span.end,
                .uri = first_span.uri,
            }
        );
        let shape = :Rule { .rule, .root = collect_values(parts, &rule) };
        Log.debug(
            () => (
                (@current Output).write("Parsed at ");
                Span.print(span);
            )
        );
        :MadeProgress {
            .shape,
            .span,
        }
    );
    
    const print_parsed_parts = (parts :: &ArrayList.t[ParsedPart]) => (
        let output = @current Output;
        for part in parts |> ArrayList.iter do (
            output.write("- ");
            match part^ with (
                | :Keyword token => output.write(token.shape |> Token.Shape.raw)
                | :Value ref ast => Ast.print(ast)
            );
            output.write("\n");
        );
    );
    
    const collect_values = (
        parsed_parts :: ArrayList.t[ParsedPart],
        rule :: &SyntaxRule.t,
    ) -> Ast.Group => (
        Log.debug_msg("Collecting values for " + escape_string(rule^.name));
        let mut parsed_part_idx = 0;
        let result = collect_values_from(
            &mut parsed_part_idx,
            &parsed_parts,
            &rule^.parts,
            .rule_group = :None,
        );
        if parsed_part_idx < &parsed_parts |> ArrayList.length then (
            print_parsed_parts(&parsed_parts);
            panic("Too many values supplied for the rule " + escape_string(rule^.name))
        ) else (
            Log.debug_msg("Values have been collected for " + escape_string(rule^.name));
            result
        )
    );
    
    const collect_values_from = (
        parsed_part_idx :: &mut Int32,
        parsed_parts :: &ArrayList.t[ParsedPart],
        rule_group_parts :: &ArrayList.t[SyntaxRule.Part],
        .rule_group :: Option.t[type (&SyntaxRule.Group)],
    ) -> Ast.Group => (
        let mut rule_part_idx = 0;
        let skip_rule_whitespace = () => (
            while rule_part_idx < rule_group_parts |> ArrayList.length do (
                match rule_group_parts^.[rule_part_idx]^ with (
                    | :Whitespace _ => ()
                    | _ => break
                );
                rule_part_idx += 1;
            );
        );
        let mut children = Tuple.new();
        while (
            parsed_part_idx^ < parsed_parts |> ArrayList.length
            and rule_part_idx < rule_group_parts |> ArrayList.length
        ) do (
            let parsed_part = parsed_parts^.[parsed_part_idx^];
            skip_rule_whitespace();
            let rule_part = rule_group_parts^.[rule_part_idx];
            match { parsed_part^, rule_part^ } with (
                | { :Value ast, :Value { .name, ... } } => (
                    Log.debug(() => (
                        let output = @current Output;
                        output.write("Matched value ");
                        Span.print(ast.span);
                    ));
                    &mut children |> Tuple.add(name, :Value ast);
                    parsed_part_idx^ += 1;
                    rule_part_idx += 1;
                )
                | { :Keyword token, :Keyword keyword } => (
                    if token.shape |> Token.Shape.raw != keyword then (
                        panic("Incorrectly matched keyword");
                    );
                    Log.debug(() => (
                        let output = @current Output;
                        output.write("Matched keyword ");
                        Token.print(token);
                    ));
                    parsed_part_idx^ += 1;
                    rule_part_idx += 1;
                )
                | { _, :Group ref rule_group } => (
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
                            let expected_keyword = match first_non_whitespace_part with (
                                | :Some &(:Keyword keyword) => keyword
                                | _ => (
                                    panic("Optional group is supposed to have keyword as first part")
                                )
                            );
                            let should_match_group = match parsed_part^ with (
                                | :Keyword token => (
                                    token.shape |> Token.Shape.raw == expected_keyword
                                )
                                | _ => false
                            );
                            if not should_match_group then (
                                rule_part_idx += 1;
                                continue;
                            );
                        )
                    );
                    Log.debug(() => (
                        let output = @current Output;
                        output.write("Matching group at ");
                        Span.print(rule_group^.span);
                    ));
                    let group = collect_values_from(
                        parsed_part_idx,
                        parsed_parts,
                        rule_group_parts,
                        .rule_group = :Some rule_group,
                    );
                    Log.debug_msg("Group matched");
                    &mut children |> Tuple.add(name, :Group group);
                    rule_part_idx += 1;
                )
                | _ => (
                    # dbg.print(parsed_part^);
                    dbg.print({ .rule_part = rule_part^ });
                    print_parsed_parts(parsed_parts);
                    panic(
                        "mismatch matching parsed part #"
                        + to_string(parsed_part_idx^)
                        + " and rule_group_part #"
                        + to_string(rule_part_idx)
                    );
                )
            );
        );
        skip_rule_whitespace();
        { .children }
    );
    
    const try_parse = (.priority_filter :: SyntaxRule.PriorityFilter) -> ParseResult => (
        let mut progress = :None;
        Log.debug_msg("Try parse");
        while try_parse_or_extend_minimal(progress, .priority_filter) is :MadeProgress ast do (
            Log.debug_msg("Made progress");
            progress = :Some ast;
        );
        Log.debug_msg("Try parse done");
        match progress with (
            | :None => :NoProgress
            | :Some ast => :MadeProgress ast
        )
    );
    
    const Parsed = newtype {
        .ast :: Ast.t,
    };
    
    const parse = (
        .ruleset :: SyntaxRuleset.t,
        .token_stream :: &mut TokenStream.t,
        .entire_source_span :: Span,
        .uri :: Uri,
    ) -> Parsed => (
        let ctx :: ContextT = {
            .ruleset,
            .token_stream,
            .entire_source_span,
            .uri,
        };
        with Context = ctx;
        let ast = match try_parse(.priority_filter = :Any) with (
            | :MadeProgress ast => ast
            | :NoProgress => {
                .shape = :Empty,
                .span = (@current Context).entire_source_span,
            }
        );
        let peek = &ctx.token_stream^ |> TokenStream.peek;
        if peek.shape is :Eof then () else (
            Error.report_msg(peek.span, "Expected eof");
        );
        {
            .ast
        }
    );
);
