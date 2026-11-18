use (import "./_common.ks").*;

const impl_for = (K :: Type) -> std.Ast => `(
    (
        ast :: &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> K => (
        let { expr, ty } = root
            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
        let ty = eval_ast_as_type(ty);
        compile[K](expr, .expected_ty = :Some ty)
    )
);

(:: CoreSyntax) {
    .name = "type ascribe",
    .compile = {
        .any_expr = include_ast impl_for(AnyExpr),
        .expr = include_ast impl_for(Expr),
        .place_expr = include_ast impl_for(PlaceExpr),
        .ty_expr = include_ast impl_for(TyExpr),
        .pattern = include_ast impl_for(Pattern),
        .assignee = include_ast impl_for(Assignee),
    },
}
