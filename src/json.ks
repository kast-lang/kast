use (import "./common.ks").*;
use (import "./output.ks").*;
use (import "./error.ks").*;
use (import "./tuple.ks").*;
use (import "./source.ks").*;
use (import "../deps/uri/src/lib.ks").*;
use (import "./lexer.ks").*;
use (import "./token.ks").*;
use (import "./token_stream.ks").*;
use (import "./position.ks").*;
use (import "./syntax_rule.ks").*;
use (import "./syntax_parser.ks").*;
use (import "./ast.ks").*;
use (import "./parser.ks").*;
use std.collections.OrdMap;
use std.Result;

module:

const Json = (
    module:
    
    const t = newtype (
        | :Number Float64
        | :String String
        | :Object OrdMap.t[String, Json.t]
        | :Array ArrayList.t[Json.t]
        | :Bool Bool
        | :Null
    );
    
    const ParseError = newtype {
        .message :: String,
        .position :: Position,
    };
    
    const ruleset = () => (
        let path = std.path.dirname(__FILE__) + "/../tests/syntax/json.ks";
        let mut lexer = Lexer.new(Source.read_file(path));
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );
    
    const parse_array = (rule :: &SyntaxRule.t, children :: Tuple.t[Ast.Child]) -> Json.t => (
        let mut elements = ArrayList.new();
        let inner = children |> Tuple.unwrap_unnamed_1 |> Ast.unwrap_child_value;
        for element in Ast.iter_list(
            inner,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :None,
        ) do (
            &mut elements |> ArrayList.push_back(from_ast(element));
        );
        :Array elements
    );
    
    const ast_to_name = (ast :: Ast.t) -> String => with_return (
        if ast.shape is :Token token then (
            match token.shape with (
                | :String { .contents, ... } => return contents
                | :Ident { .name, ... } => return name
                | _ => ()
            );
        );
        Error.report_and_unwind(
            ast.span,
            () => (
                let output = @current Output;
                output.write("Expected field name");
            ),
        )
    );
    
    const parse_obj = (rule :: &SyntaxRule.t, children :: Tuple.t[Ast.Child]) -> Json.t => (
        let mut fields = OrdMap.new();
        let inner = children |> Tuple.unwrap_unnamed_1 |> Ast.unwrap_child_value;
        for field in Ast.iter_list(
            inner,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :None,
        ) do (
            if field.shape is :Rule { .rule, .root = { .children, ... } } then (
                if rule.name == "field" then (
                    let name = (&children |> Tuple.get_named("name") |> Option.unwrap)^
                        |> Ast.unwrap_child_value;
                    let value = (&children |> Tuple.get_named("value") |> Option.unwrap)^
                        |> Ast.unwrap_child_value;
                    let name = ast_to_name(name);
                    let value = from_ast(value);
                    &mut fields |> OrdMap.add(name, value);
                    continue;
                );
            );
            Error.report_and_unwind(
                field.span,
                () => (
                    let output = @current Output;
                    output.write("Expected field, got ");
                    output.write(rule^.name);
                )
            );
        );
        :Object fields
    );
    
    const from_ast = (ast :: Ast.t) -> Json.t => with_return (
        match ast.shape with (
            | :Empty => Error.report_and_unwind(
                ast.span,
                () => (@current Output).write("json can't be empty"),
            )
            | :Token token => (
                match token.shape with (
                    | :String { .contents, ... } => :String contents
                    | :Number { .raw, ... } => :Number std.String.parse[Float64](raw)
                    | _ => Error.report_and_unwind(
                        token.span,
                        () => (
                            (@current Output).write("json can't be ");
                            Token.Shape.print(token.shape);
                        )
                    )
                )
            )
            | :Rule { .rule, .root = { .children, ... } } => (
                if rule.name == "obj" then (
                    return parse_obj(&rule, children);
                );
                if rule.name == "array" then (
                    return parse_array(&rule, children);
                );
                if rule.name == "null" then (
                    return :Null;
                );
                if rule.name == "true" then (
                    return :Bool true;
                );
                if rule.name == "false" then (
                    return :Bool false;
                );
                Error.report_and_unwind(
                    ast.span,
                    () => (
                        let output = @current Output;
                        output.write(rule.name);
                        output.write(" is not valid json");
                    ),
                )
            )
        )
    );
    
    const parse = (json :: String) -> Result.t[Json.t, ParseError] => (
        let uri :: Uri = Uri.new_path("<json>");
        let source :: Source = {
            .contents = json,
            .uri,
        };
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset = ruleset(),
            .token_stream = &mut token_stream,
            .entire_source_span = Source.entire_span(&source),
            .uri,
        );
        :Ok from_ast(parsed.ast)
    );

    const print = (self :: &Json.t) => (
        match self^ with (
            | :Number value => (
                ansi.with_mode(
                    :Italic,
                    () => (@current Output).write(to_string(value)),
                );
            )
            | :String s => (
                ansi.with_mode(
                    :Green,
                    () => (@current Output).write(escape_string(s)),
                );
            )
            | :Object ref fields => (
                let output = @current Output;
                output.write("{");
                let mut is_empty = true;
                for { i, field } in fields |> OrdMap.iter |> std.iter.enumerate do (
                    is_empty = false;
                    if i != 0 then output.write(",");
                    output.write("\n");
                    if i == 0 then (
                        output.inc_indentation();
                    );
                    print(&(:String field^.key));
                    output.write(": ");
                    print(&field^.value);
                );
                if is_empty then (
                    output.write(" ");
                ) else (
                    output.write("\n");
                    output.dec_indentation();
                );
                output.write("}");
            )
            | :Array ref a => (
                let output = @current Output;
                output.write("[");
                let mut is_empty = true;
                for { i, element } in a |> ArrayList.iter |> std.iter.enumerate do (
                    is_empty = false;
                    if i != 0 then output.write(",");
                    output.write("\n");
                    if i == 0 then (
                        output.inc_indentation();
                    );
                    print(element);
                );
                if is_empty then (
                    output.write(" ");
                ) else (
                    output.write("\n");
                    output.dec_indentation();
                );
                output.write("]");
            )
            | :Bool value => (
                ansi.with_mode(
                    :Magenta,
                    () => (@current Output).write(to_string(value)),
                );
            )
            | :Null => (
                ansi.with_mode(
                    :Magenta,
                    () => (@current Output).write("null"),
                );
            )
        );
    );
);
