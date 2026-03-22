use (import "./common.ks").*;
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

@syntax "index_array" 70 @wrap never = <- array "." "[" index:any "]";
impl syntax (array.[index]) = `(
    &$array |> ArrayList.at($index)
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
    ) -> ParseResult => with_return (
        if starting_value is :None then (
            if try_parse_single_token() is :MadeProgress ast then (
                return :MadeProgress ast;
            );
        );
        try_parse_single_rule_node(starting_value)
    );
    
    const skip_comments_and_errors = () => (
        # TODO
    );
    
    const try_parse_single_token = () -> ParseResult => (
        let ctx = @current Context;
        let peek = &ctx.token_stream^ |> TokenStream.peek;
        let peek_raw = peek.shape |> Token.Shape.raw;
        let do_parse = match peek.shape with (
            | :Comment _ => panic("unreachable")
            | :Punct _ => false
            | :Ident _ => true
            | :String _ => true
            | :Number _ => true
            | :Eof => false
            | :Error _ => panic("unreachable")
        );
        if do_parse then (
            Log.debug("Parsed single token " + escape_string(peek_raw));
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
    
    const try_parse_single_rule_node = (starting_value :: Option.t[Ast.t]) -> ParseResult => with_return (
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
            let peek = &ctx.token_stream^ |> TokenStream.peek;
            let peek_raw = peek.shape |> Token.Shape.raw;
            
            # Try to follow with keyword
            if &node^.next |> OrdMap.get(:Keyword peek_raw) is :Some edge then (
                ctx.token_stream |> TokenStream.advance;
                Log.debug("Following with keyword " + escape_string(peek_raw));
                node = &edge^.target;
                made_progress = true;
                &mut parts |> ArrayList.push_back(:Keyword peek);
                continue;
            );
            
            # If no progress was made,
            # then we can start with a simple token value
            # Otherwise this would be infinite recursion
            if &parts |> ArrayList.length != 0 then (
                # Try to follow with value
                if &node^.next |> OrdMap.get(:Value) is :Some edge then (
                    if try_parse() is :MadeProgress ast then (
                        Log.debug("Following with value");
                        node = &edge^.target;
                        made_progress = true;
                        &mut parts |> ArrayList.push_back(:Value ast);
                        continue;
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
                    panic("Can't finish parsing")
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
        :MadeProgress {
            .shape = :Rule { .rule, .root = collect_values(parts, &rule.parts) },
            .span,
        }
    );
    
    const collect_values = (
        parsed_parts :: ArrayList.t[ParsedPart],
        rule_parts :: &ArrayList.t[SyntaxRule.Part],
    ) -> Ast.Group => (
        let mut rule_part_idx = 0;
        let skip_rule_whitespace = () => (
            while rule_part_idx < rule_parts |> ArrayList.length do (
                match rule_parts^.[rule_part_idx]^ with (
                    | :Whitespace _ => ()
                    | _ => break
                );
                rule_part_idx += 1;
            );
        );
        let mut children = Tuple.new();
        for &parsed_part in &parsed_parts |> ArrayList.iter do (
            skip_rule_whitespace();
            let rule_part = if rule_part_idx < rule_parts |> ArrayList.length then (
                rule_parts^.[rule_part_idx]
            ) else (
                panic("Too many values")
            );
            match { parsed_part, rule_part^ } with (
                | {:Value ast, :Value { .name, ... }} => (
                    &mut children |> Tuple.add(name, :Value ast);
                )
                | { :Keyword token, :Keyword keyword } => (
                    if token.shape |> Token.Shape.raw != keyword then (
                        panic("Incorrectly matched keyword");
                    )
                )
                | _ => (
                    dbg.print(children);
                    panic("mismatch")
                )
            );
            rule_part_idx += 1;
        );
        skip_rule_whitespace();
        { .children }
    );
    
    const try_parse = () -> ParseResult => (
        let mut progress = :None;
        Log.debug("Try parse");
        while try_parse_or_extend_minimal(progress) is :MadeProgress ast do (
            Log.debug("Made progress");
            progress = :Some ast;
        );
        Log.debug("Try parse done");
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
        let ast = match try_parse() with (
            | :MadeProgress ast => ast
            | :NoProgress => {
                .shape = :Empty,
                .span = (@current Context).entire_source_span,
            }
        );
        let peek = &ctx.token_stream^ |> TokenStream.peek;
        if peek.shape is :Eof then () else (
            Error.report(peek.span, "Expected eof");
        );
        {
            .ast
        }
    );
);
