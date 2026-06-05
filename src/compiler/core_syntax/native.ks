use (import "./_common.ks").*;

const compile_native = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let ty = match expected_ty with (
        | :Some ty => ty
        | :None => (
            let diagnostic = {
                .severity = :Error,
                .source = :Compiler,
                .span = ast^.span,
                .message = () => (
                    let output = @current Output;
                    output.write("Could not infer native expr type");
                ),
                .related = ArrayList.new(),
            };
            Diagnostic.report_and_unwind(diagnostic)
        )
    );
    let s = root^
        |> AstHelpers.expect_single_child(:None)
        |> AstHelpers.expect_string_literal;
    {
        .shape = :Expr :Native s,
        .ty,
    }
);

include_ast impl_any_expr_syntax(
    "native",
    compile_native,
)
