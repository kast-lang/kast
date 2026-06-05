use (import "./_common.ks").*;

const compile_stmt = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let expr = root^
        |> AstHelpers.expect_single_child(:None);
    {
        .shape = :Expr :Stmt compile[Expr](&expr, .expected_ty = :None),
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "stmt",
    compile_stmt,
)
