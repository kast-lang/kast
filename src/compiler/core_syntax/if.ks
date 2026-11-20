use (import "./_common.ks").*;

const compile_if = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { condition, then_case, else_case } = root
        |> AstHelpers.expect_three_children(
            "condition",
            "then_case",
            "else_case",
        );
    let condition = compile[Expr](condition, .expected_ty = :Some Ty.BOOL);
    let then_case = compile[Expr](then_case, .expected_ty);
    let result_ty = then_case^.ty;
    let else_case = compile[Expr](else_case, .expected_ty = :Some result_ty);
    {
        .shape = :Expr :If {
            .condition,
            .then_case,
            .else_case,
        },
        .ty = result_ty,
    }
);

include_ast impl_any_expr_syntax(
    "if",
    compile_if,
)
