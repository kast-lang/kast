use (import "./common.ks").*;
use (import "./template.ks").*;

module:

const find_context_type = (name :: &RcStr, .span :: Span) -> Ir.Type => with_return (
    if (@current Compiler).get_toplevel_impl(name) is :Some item then (
        if item^ is :Context ty then (
            return ty;
        );
        let decl = (@current Compiler).get_toplevel_decl(name) |> Option.unwrap;
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_escaped(name);
                locked_output.write_str("is ");
                print_toplevel_kind(decl);
                locked_output.write_str(", not a context");
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
            locked_output.write_str("Context ");
            locked_output.write_escaped(name);
            locked_output.write_str(" not found");
        ),
        .span,
        .related = ArrayList.new(),
    };
    Diagnostic.report_and_unwind(diagnostic)
);

const parse_context_type = (ast :: &Ast.t) -> { RcStr, Ir.Type } => (
    let context_name = with_return (
        match ast^.shape^ with (
            | :Rule { .rule = ref rule, .root = ref root } => (
                if &rule^^.name |> RcStr.as_str == "instantiate" then (
                    return parse_instantiate(root).name;
                );
            )
            | :Token ref token => (
                if token^.shape^ is :Ident { .name, ... } then (
                    return name;
                );
            )
        );
        let diagnostic = {
            .severity = :Error,
            .source = :Compiler,
            .message = () => (
                locked_output.write_str("Expected context name");
            ),
            .span = ast^.span,
            .related = ArrayList.new(),
        };
        Diagnostic.report_and_unwind(diagnostic)
    );
    { context_name, find_context_type(&context_name, .span = ast^.span) }
);
