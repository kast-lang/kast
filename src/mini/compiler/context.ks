use (import "./common.ks").*;

module:

const parse_context_type = (ast :: Ast.t) -> { String, Ir.Type } => (
    let context_name = ast |> AstHelpers.expect_ident;
    let context_ty = match &(@current Compiler).program.contexts |> OrdMap.get(context_name) with (
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
                .span = ast.span,
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );
    { context_name, context_ty }
);
