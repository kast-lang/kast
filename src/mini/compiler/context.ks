use (import "./common.ks").*;

module:

const find_context_type = (context_name :: String, .span :: Span) -> Ir.Type => (
    match &(@current Compiler).program.contexts |> OrdMap.get(context_name) with (
        | :Some &ty => ty
        | :None => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .message = () => (
                    let output = @current Output;
                    output.write("Unknown context ");
                    output.write(String.escape(context_name));
                ),
                .span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    )
);

const parse_context_type = (ast :: Ast.t) -> { String, Ir.Type } => (
    let context_name = ast |> AstHelpers.expect_ident;
    { context_name, find_context_type(context_name, .span = ast.span) }
);
