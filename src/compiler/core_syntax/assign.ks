use (import "./_common.ks").*;

const compile_assign = (
    ast :: &Ast.t,
    .root :: &Ast.Group,
    .expected_ty :: Option.t[Ty],
) -> AnyExpr => (
    let { assignee, value } = root^
        |> AstHelpers.expect_two_children(:Some { "assignee", "value" });
    let assignee = compile[Assignee](&assignee, .expected_ty = :None);
    let value = compile[PlaceExpr](&value, .expected_ty = :Some assignee.ty);
    {
        .shape = :Expr :Assign {
            .assignee,
            .value,
        },
        .ty = Ty.UNIT,
    }
);

include_ast impl_any_expr_syntax(
    "assign",
    compile_assign,
)
