use (import "./_common.ks").*;

const compile_typeof = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
) -> TyExpr => (
    let expr = root
        |> AstHelpers.expect_single_child(:None);
    let expr = compile[AnyExpr](expr, .expected_ty = :None);
    let ty = expr.ty;
    Rc.new({
        .shape = :Const ty,
        .span = ast^.span,
    })
);

include_ast impl_type_expr_syntax(
    "typeof",
    compile_typeof,
)
