use (import "../output.ks").*;
use (import "../span.ks").*;
use (import "../diagnostic.ks").*;
use (import "../tuple.ks").*;
use (import "../source.ks").*;
use (import "../source_path.ks").*;
use (import "../../deps/uri/src/lib.ks").*;
use (import "../lexer/_lib.ks").*;
use (import "../token.ks").*;
use (import "../token_stream.ks").*;
use (import "../position.ks").*;
use (import "../syntax_rule.ks").*;
use (import "../syntax_parser.ks").*;
use (import "../ast.ks").*;
use (import "../parser.ks").*;
use (import "../rc_str.ks").*;
const dep = import "../../deps/json/lib.ks";
use std.collections.OrdMap;

const error = [T] (span :: Span, message :: () -> ()) -> T => (
    let diagnostic = {
        .severity = :Error,
        .span,
        .source = :Parser,
        .message,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

module:

const Json = (
    module:

    const t = newtype (
        | :Number Float64
        | :String RcStr
        | :Object OrdMap.t[RcStr, Json.t]
        | :Array ArrayList.t[Json.t]
        | :Bool Bool
        | :Null
    );

    const from_dep = (value :: dep.Value) -> Json.t => (
        match value with (
            | :Number x => (
                let x = dep.Number.to_f64(&x);
                :Number x
            )
            | :String s => :String RcStr.from_String(s)
            | :Object a => (
                let mut b = OrdMap.new();
                for { name, value } in a |> ArrayList.into_iter do (
                    let value = from_dep(value);
                    &mut b |> OrdMap.add(RcStr.from_String(name), value);
                );
                :Object b
            )
            | :Array a => (
                let mut b = ArrayList.new();
                for element in a |> ArrayList.into_iter do (
                    &mut b |> ArrayList.push_back(from_dep(element));
                );
                :Array b
            )
            | :Bool x => :Bool x
            | :Null => :Null
        )
    );

    const into_dep = (value :: Json.t) -> dep.Value => (
        match value with (
            | :Number x => (
                let x = dep.Number.parse(&mut dep.Reader.create(&to_string(x) |> as_str)) |> Option.unwrap;
                :Number x
            )
            | :String s => :String String.from_str(&s |> RcStr.as_str)
            | :Object a => (
                let mut b = ArrayList.new();
                for { .key = name, .value = value } in a |> OrdMap.into_iter do (
                    let name = String.from_str(&name |> RcStr.as_str);
                    let value = into_dep(value);
                    &mut b |> ArrayList.push_back({ name, value });
                );
                :Object b
            )
            | :Array a => (
                let mut b = ArrayList.new();
                for element in a |> ArrayList.into_iter do (
                    &mut b |> ArrayList.push_back(into_dep(element));
                );
                :Array b
            )
            | :Bool x => :Bool x
            | :Null => :Null
        )
    );

    const ParseError = newtype {
        .message :: String,
        .position :: Position,
    };

    const ruleset = () => (
        let path = "kast:///json/syntax.ks";
        let source = Source.read(&SourcePath.parse(path));
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        SyntaxParser.parse_syntax_ruleset(&mut token_stream)
    );

    const parse_array = (rule :: &SyntaxRule.t, children :: &Tuple.t[Ast.Child]) -> Json.t => (
        let mut elements = ArrayList.new();
        let inner = children |> Tuple.get_unnamed(0) |> Ast.unwrap_child_value;
        for element in Ast.iter_list(
            inner,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :None,
        ) do (
            &mut elements |> ArrayList.push_back(from_ast(element));
        );
        :Array elements
    );

    const ast_to_name = (ast :: &Ast.t) -> RcStr => with_return (
        if ast^.shape^ is :Token ref token then (
            match token^.shape^ with (
                | :String { .contents, ... } => return contents
                | :Ident { .name, ... } => return name
                | _ => ()
            );
        );
        error(
            ast^.span,
            () => (
                let output = @current Output;
                output.write("Expected field name");
            ),
        )
    );

    const parse_obj = (rule :: &SyntaxRule.t, children :: &Tuple.t[Ast.Child]) -> Json.t => (
        let mut fields = OrdMap.new();
        let inner = children |> Tuple.get_unnamed(0) |> Ast.unwrap_child_value;
        for field in Ast.iter_list(
            inner,
            .binary_rule_name = "comma",
            .trailing_or_leading_rule_name = :None,
        ) do (
            if field^.shape^ is :Rule { .rule = ref rule, .root = ref root } then (
                if &rule^^.name |> RcStr.as_str == "field" then (
                    let name = &root^.children
                        |> Tuple.get_named("name")
                        |> Ast.unwrap_child_value;
                    let value = &root^.children
                        |> Tuple.get_named("value")
                        |> Ast.unwrap_child_value;
                    let name = ast_to_name(name);
                    let value = from_ast(value);
                    &mut fields |> OrdMap.add(name, value);
                    continue;
                );
            );
            error(
                field^.span,
                () => (
                    write_str("Expected field, got ");
                    write(&rule^.name);
                )
            );
        );
        :Object fields
    );

    const from_ast = (ast :: &Ast.t) -> Json.t => with_return (
        match ast^.shape^ with (
            | :Empty => error(
                ast^.span,
                () => (@current Output).write("json can't be empty"),
            )
            | :Token ref token => (
                match token^.shape^ with (
                    | :String { .contents = ref contents, ... } => :String contents^
                    | :Number { .raw = ref raw, ... } => :Number std.String.parse[Float64](raw |> RcStr.as_str)
                    | _ => error(
                        token^.span,
                        () => (
                            (@current Output).write("json can't be ");
                            Token.Shape.print(&token^.shape^);
                        )
                    )
                )
            )
            | :InterpolatedString _ => (
                error(
                    ast^.span,
                    () => (@current Output).write("Interpolated strings aren't supported")
                )
            )
            | :Rule { .rule = ref rule, .root = { .children = ref children, ... } } => (
                let rule = &rule^^;
                if &rule^.name |> RcStr.as_str == "negative" then (
                    let inner = children |> Tuple.get_unnamed(0) |> Ast.unwrap_child_value;
                    let value = from_ast(inner);
                    match value with (
                        | :Number x => return :Number -x
                        | _ => (
                            error(
                                inner^.span,
                                () => (@current Output).write("Expected a number to be negated"),
                            )
                        )
                    )
                );
                if &rule^.name |> RcStr.as_str == "obj" then (
                    return parse_obj(rule, children);
                );
                if &rule^.name |> RcStr.as_str == "array" then (
                    return parse_array(rule, children);
                );
                if &rule^.name |> RcStr.as_str == "null" then (
                    return :Null;
                );
                if &rule^.name |> RcStr.as_str == "true" then (
                    return :Bool true;
                );
                if &rule^.name |> RcStr.as_str == "false" then (
                    return :Bool false;
                );
                error(
                    ast^.span,
                    () => (
                        write(&rule^.name);
                        write_str(" is not valid json");
                    ),
                )
            )
        )
    );

    const parse = (json :: &str) -> Result.t[Json.t, ParseError] => (
        let path :: SourcePath = :Special RcStr.from_String(String.from_str("<json>"));
        let source :: Source = {
            .contents = RcStr.from_String(String.from_str(json)),
            .path,
        };
        let mut lexer = Lexer.new(source);
        let mut token_stream = TokenStream.from_fn(() => Lexer.next(&mut lexer));
        let parsed = Parser.parse(
            .ruleset = &mut ruleset(),
            .token_stream = &mut token_stream,
            .entire_source_span = Source.entire_span(&source),
            .path,
        );
        :Ok from_ast(&parsed.ast)
    );

    const to_str = (self :: &Json.t) -> String => (
        output_to_string(() => print(self))
    );

    const print = (self :: &Json.t) => (
        match self^ with (
            | :Number value => (
                ansi.with_mode(
                    :Italic,
                    () => write_str(&to_string(value) |> as_str),
                );
            )
            | :String ref s => (
                ansi.with_mode(
                    :Green,
                    () => write_escaped(s),
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
                    () => (@current Output).write(&to_string(value) |> as_str),
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
