use (import "./output.ks").*;
use (import "./ansi.ks").*;
use (import "./span.ks").*;
use (import "./common.ks").*;
use (import "./error.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./log.ks").*;

module:

const SyntaxParser = (
    module:
    
    const expect_and_advance = (
        tokens :: &mut TokenStream.t,
        expected_raw :: String,
    ) => (
        let actual = &tokens^ |> TokenStream.peek;
        let actual_raw = actual.shape |> Token.Shape.raw;
        if actual_raw != expected_raw then (
            Error.report_and_unwind(
                actual.span,
                () => (
                    let output = @current Output;
                    output.write("Expected ");
                    ansi.with_mode(
                        :Magenta,
                        () => output.write(expected_raw),
                    );
                    output.write(", got ");
                    actual.shape |> Token.Shape.print;
                ),
            );
        ) else (
            tokens |> TokenStream.advance;
        );
    );
    
    const peek_is = (
        tokens :: &mut TokenStream.t,
        raw :: String,
    ) -> Bool => (
        let peek = &tokens^ |> TokenStream.peek;
        peek.shape |> Token.Shape.raw == raw
    );
    
    const SyntaxCommand = newtype (
        | :FromScratch
        | :Rule SyntaxRule.t
    );
    
    const read_wrap_mode = (
        token_stream :: &mut TokenStream.t,
    ) -> SyntaxRule.WrapMode => with_return (
        if token_stream |> peek_is("if_any") then (
            token_stream |> TokenStream.advance;
            return :IfAnyNonAssoc;
        );
        if token_stream |> peek_is("if_any_assoc") then (
            token_stream |> TokenStream.advance;
            return :IfAnyAssoc;
        );
        if token_stream |> peek_is("never") then (
            token_stream |> TokenStream.advance;
            return :Never;
        );
        if token_stream |> peek_is("always") then (
            token_stream |> TokenStream.advance;
            return :Always;
        );
        let peek = &token_stream^ |> TokenStream.peek;
        Error.report_and_unwind(
            peek.span,
            () => (
                let output = @current Output;
                output.write("Expected wrap mode (never/always/if_any/if_any_assoc), got ");
                peek.shape |> Token.Shape.print;
            ),
        )
    );
    
    const read_group = (
        token_stream :: &mut TokenStream.t,
        .name :: Option.t[String],
        .rule_priority :: SyntaxRule.Priority,
    ) -> SyntaxRule.Group => with_return (
        token_stream |> expect_and_advance("(");
        let mut parts = ArrayList.new[SyntaxRule.Part]();
        let wrap_mode :: Option.t[SyntaxRule.WrapMode] = if token_stream |> peek_is("@wrap") then (
            token_stream |> TokenStream.advance;
            :Some read_wrap_mode(token_stream)
        ) else (
            :None
        );
        while not token_stream |> peek_is(")") do (
            let part = read_part(token_stream, .rule_priority, .top_level = :None);
            &mut parts |> ArrayList.push_back(part);
        );
        token_stream |> expect_and_advance(")");
        let quantifier = if token_stream |> peek_is("?") then (
            token_stream |> TokenStream.advance;
            :Optional
        ) else (
            :None
        );
        {
            .name,
            .parts,
            .quantifier,
            .wrap_mode,
        }
    );
    
    const TopLevelState = newtype {
        .first :: Bool,
        .right_assoc :: Bool,
    };
    
    const read_priority = (
        token_stream :: &mut TokenStream.t,
    ) -> SyntaxRule.Priority => (
        let peek = &token_stream^ |> TokenStream.peek;
        if peek.shape is :Number { .raw, ... } then (
            token_stream |> TokenStream.advance;
            raw |> parse
        ) else (
            Error.report_and_unwind(
                peek.span,
                () => (
                    let output = @current Output;
                    output.write("Expected syntax rule priority (number token), got ");
                    peek.shape |> Token.Shape.print;
                ),
            )
        )
    );
    
    const read_part = (
        token_stream :: &mut TokenStream.t,
        .rule_priority :: SyntaxRule.Priority,
        .top_level :: Option.t[type (&mut TopLevelState)],
    ) -> SyntaxRule.Part => with_return (
        let mut left_assoc = false;
        if top_level is :Some top_level then (
            if token_stream |> peek_is("<-") then (
                Log.trace("<-");
                if not top_level^.first then (
                    let peek = &token_stream^ |> TokenStream.peek;
                    Error.report_and_unwind(
                        peek.span,
                        () => (
                            let output = @current Output;
                            output.write("\"<-\" can only be specified in beginning of the rule");
                        ),
                    );
                );
                left_assoc = true;
                token_stream |> TokenStream.advance;
            );
        );
        let peek = &token_stream^ |> TokenStream.peek;
        match peek.shape with (
            | :Ident { .name, ... } => (
                let name = if name == "_" then (
                    :None
                ) else (
                    :Some name
                );
                token_stream |> TokenStream.advance;
                
                if token_stream |> peek_is("=") then (
                    token_stream |> TokenStream.advance;
                    return :Group read_group(token_stream, .name, .rule_priority);
                );
                
                let mut right_assoc = false;
                if top_level is :Some top_level then (
                    if token_stream |> peek_is("->") then (
                        top_level^.right_assoc = true;
                        right_assoc = true;
                        token_stream |> TokenStream.advance;
                    );
                );
                
                let priority_filter = if token_stream |> peek_is(":") then (
                    token_stream |> TokenStream.advance;
                    if token_stream |> peek_is("any") then (
                        token_stream |> TokenStream.advance;
                        :Any
                    ) else if token_stream |> peek_is(">") then (
                        token_stream |> TokenStream.advance;
                        :Greater read_priority(token_stream)
                    ) else if token_stream |> peek_is(">=") then (
                        token_stream |> TokenStream.advance;
                        :GreaterOrEqual read_priority(token_stream)
                    ) else (
                        let peek = &token_stream^ |> TokenStream.peek;
                        Error.report_and_unwind(
                            peek.span,
                            () => (
                                let output = @current Output;
                                output.write("Expected priority filter (\"any\" or \">\" or \">=\"), got ");
                                peek.shape |> Token.Shape.print;
                            ),
                        )
                    )
                ) else if left_assoc or right_assoc then (
                    :GreaterOrEqual rule_priority
                ) else (
                    :Greater rule_priority
                );
                return :Value {
                    .name,
                    .priority_filter,
                };
            )
            | :String { .contents, ... } => (
                if String.is_whitespace(contents) then (
                    let no_wrap = contents;
                    token_stream |> TokenStream.advance;
                    let wrap = if token_stream |> peek_is("/") then (
                        token_stream |> TokenStream.advance;
                        let peek = &token_stream^ |> TokenStream.peek;
                        if peek.shape is :String { .contents, ... } then (
                            token_stream |> TokenStream.advance;
                            contents
                        ) else (
                            Error.report_and_unwind(
                                peek.span,
                                () => (
                                    let output = @current Output;
                                    output.write("Expected whitespace when wrapped (string token), got ");
                                    peek.shape |> Token.Shape.print;
                                ),
                            )
                        )
                    ) else no_wrap;
                    return :Whitespace { .no_wrap, .wrap };
                ) else (
                    token_stream |> TokenStream.advance;
                    return :Keyword contents;
                )
            )
            | _ => (
                Error.report_and_unwind(
                    peek.span,
                    () => (
                        let output = @current Output;
                        output.write("Expected syntax rule part, got ");
                        peek.shape |> Token.Shape.print;
                    ),
                )
            )
        );
        
        panic("unreachable")
    );
    
    const parse_syntax_rule = (
        token_stream :: &mut TokenStream.t,
    ) -> SyntaxRule.t => (
        let { .start = span_start, .uri, ... } = (&token_stream^ |> TokenStream.peek).span;
        
        let name :: String = (
            let peek = &token_stream^ |> TokenStream.peek;
            if peek.shape is :String { .contents, ... } then (
                token_stream |> TokenStream.advance;
                contents
            ) else (
                Error.report_and_unwind(
                    peek.span,
                    () => (
                        let output = @current Output;
                        output.write("Expected syntax rule name (string token), got ");
                        peek.shape |> Token.Shape.print;
                    ),
                )
            )
        );
        
        let rule_priority = read_priority(token_stream);
        let wrap_mode :: SyntaxRule.WrapMode = (
            token_stream |> expect_and_advance("@wrap");
            read_wrap_mode(token_stream)
        );
        token_stream |> expect_and_advance("=");
        
        let mut parts = ArrayList.new[SyntaxRule.Part]();
        let mut top_level :: TopLevelState = {
            .first = true,
            .right_assoc = false,
        };
        while not token_stream |> peek_is(";") do (
            if top_level.right_assoc then (
                let peek = &token_stream^ |> TokenStream.peek;
                Error.report_and_unwind(
                    peek.span,
                    () => (
                        let output = @current Output;
                        output.write("Expected \";\" to finish rule, got ");
                        peek.shape |> Token.Shape.print;
                    ),
                )
            );
            let part = read_part(
                token_stream,
                .rule_priority,
                .top_level = :Some &mut top_level,
            );
            &mut parts |> ArrayList.push_back(part);
            top_level.first = false;
        );
        
        let span_end = (&token_stream^ |> TokenStream.peek).span.start;
        {
            .name,
            .priority = rule_priority,
            .parts,
            .wrap_mode,
            .span = {
                .start = span_start,
                .end = span_end,
                .uri,
            }
        }
    );
    
    const parse_syntax_command = (
        token_stream :: &mut TokenStream.t,
    ) -> SyntaxCommand => (
        if token_stream |> peek_is("from_scratch") then (
            token_stream |> TokenStream.advance;
            :FromScratch
        ) else (
            :Rule parse_syntax_rule(token_stream)
        )
    );
    
    const parse_syntax_rules = (
        token_stream :: &mut TokenStream.t,
    ) -> ArrayList.t[SyntaxRule.t] => (
        let mut result = ArrayList.new();
        loop (
            let peek = &token_stream^ |> TokenStream.peek;
            let index_before = token_stream^.index;
            with Error.UnwindableHandler = {
                .unwind_on_error = [T] () => (
                    if token_stream^.index == index_before then (
                        Log.debug("skipping " + escape_string(Token.Shape.raw(peek.shape)));
                        token_stream |> TokenStream.advance;
                    );
                    continue
                ),
            };
            Log.trace("parse_syntax_rules: peek = " + escape_string(Token.Shape.raw(peek.shape)));
            if peek.shape is :Eof then break;
            if peek.shape is :Comment _ then (
                token_stream |> TokenStream.advance;
                continue;
            );
            token_stream |> expect_and_advance("@syntax");
            match parse_syntax_command(token_stream) with (
                | :FromScratch => (
                    Log.debug("parsed from_scratch");
                )
                | :Rule rule => (
                    Log.debug("parsed rule " + escape_string(rule.name));
                    &mut result |> ArrayList.push_back(rule);
                )
            );
            token_stream |> expect_and_advance(";");
        );
        result
    );
);
