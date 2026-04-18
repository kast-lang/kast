use (import "../common.ks").*;
use (import "../template.ks").*;

module:

const lookup_type = (name :: String, .span :: Span) -> Ir.Type => with_return (
    if &(@current TemplateArgsContext).by_name |> OrdMap.get(name) is :Some ty then (
        return ty^;
    );
    if (@current Compiler).get_toplevel_decl(name) is :Some decl then (
        if decl is :Type then (
            return :Named name;
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write(String.escape(name));
                output.write("is ");
                print_toplevel_kind(decl);
                output.write(", not a type");
            ),
            .span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            let output = @current Output;
            output.write("Type ");
            output.write(String.escape(name));
            output.write(" not found");
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const parse_type = (ast :: Ast.t) -> Ir.Type => with_return (
    match ast.shape with (
        | :Empty => return :Unit
        | :Rule { .rule, .root } => (
            if rule.name == "instantiate" then (
                return parse_instantiate_ty(root);
            );
            if rule.name == "native" then (
                let raw = root
                    |> AstHelpers.expect_single_child(:None)
                    |> AstHelpers.expect_string;
                return :Native raw;
            );
            if rule.name == "ref" then (
                let referenced = root |> AstHelpers.expect_single_child(:None);
                return :Ref parse_type(referenced);
            );
            if rule.name == "fn_type" then (
                let call_convention = match (
                    &root.children
                        |> Tuple.get_named_opt("call_convention")
                ) with (
                    | :None => :None
                    | :Some &child => :Some (
                        child
                            |> Ast.unwrap_child_group
                            |> AstHelpers.expect_single_child(:None)
                            |> AstHelpers.expect_string
                    )
                );
                let arg_asts = (
                    &root.children
                        |> Tuple.get_named("args")
                )^
                    |> Ast.unwrap_child_value;
                let result = (
                    &root.children
                        |> Tuple.get_named("result")
                )^
                    |> Ast.unwrap_child_value;
                let arg_asts = arg_asts
                    |> AstHelpers.expect_rule("scope")
                    |> AstHelpers.expect_single_child(:None);
                let mut args = ArrayList.new();
                for arg_ast in Ast.iter_list(
                    arg_asts,
                    .binary_rule_name = "comma",
                    .trailing_or_leading_rule_name = :Some "trailing comma",
                ) do (
                    if arg_ast.shape is :Rule { .rule, .root } then (
                        if rule.name == "type ascribe" then (
                            let { name, ty } = root
                                |> AstHelpers.expect_two_children(:Some { "expr", "type" });
                            let name = name |> AstHelpers.expect_ident;
                            &mut args |> ArrayList.push_back(parse_type(ty));
                            continue;
                        );
                    );
                    &mut args |> ArrayList.push_back(parse_type(arg_ast));
                );
                let result = parse_type(result);
                return :Fn { .call_convention, .args, .result };
            );
            if rule.name == "scope" then (
                let inner = root
                    |> AstHelpers.expect_single_child(:None);
                return parse_type(inner);
            );
        )
        | :Token token => match token.shape with (
            | :Ident ident => (
                let name = ident.name;
                if name == "Any" then return :Any;
                if name == "Unit" then return :Unit;
                if name == "Bool" then return :Bool;
                if name == "Int" then return :Int;
                if name == "UInt" then return :UInt;
                if name |> String.strip_prefix(.prefix = "UInt") is :Some bits then (
                    return :IntSpecific {
                        .signed = false,
                        .bits = parse(bits),
                    };
                );
                if name |> String.strip_prefix(.prefix = "Int") is :Some bits then (
                    return :IntSpecific {
                        .signed = true,
                        .bits = parse(bits),
                    };
                );
                if name == "Float32" then return :Float32;
                if name == "Float64" then return :Float64;
                if name == "Char" then return :Char;
                return lookup_type(name, .span = token.span);
            )
            | _ => ()
        )
        | _ => ()
    );
    let diagnostic = {
        .severity = :Error,
        .source = :Compiler,
        .message = () => (
            (@current Output).write("Expected a type, got ");
            Highlight.print_single_line(&ast);
        ),
        .span = ast.span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);
