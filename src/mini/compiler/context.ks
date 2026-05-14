use (import "./common.ks").*;
use (import "./template.ks").*;

module:

const find_context_type = (name :: String, .span :: Span) -> Ir.Type => with_return (
    if (@current Compiler).get_toplevel_impl(name) is :Some item then (
        if item is :Context ty then (
            return ty;
        );
        let decl = (@current Compiler).get_toplevel_decl(name) |> Option.unwrap;
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write(String.escape(name));
                output.write("is ");
                print_toplevel_kind(decl);
                output.write(", not a context");
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
            output.write("Context ");
            output.write(String.escape(name));
            output.write(" not found");
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const parse_context_type = (ast :: Ast.t) -> { String, Ir.Type } => (
    let context_name = with_return (
        match ast.shape with (
            | :Rule { .rule, .root } => (
                if rule.name == "instantiate" then (
                    return parse_instantiate(root).name;
                );
            )
            | :Token { .shape = :Ident { .name, ... }, ... } => return name
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                let output = @current Output;
                output.write("Expected context name");
            ),
            .span = ast.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    { context_name, find_context_type(context_name, .span = ast.span) }
);
