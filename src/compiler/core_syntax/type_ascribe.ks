use (import "./_common.ks").*;

(:: CoreSyntax) {
    .name = "type ascribe",
    .compile = [K] (
        ast :: &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> K => (
        let { expr, ty } = root^
            |> AstHelpers.expect_two_children(:Some { "expr", "type" });
        let ty = eval_ast_as_type(&ty);
        compile[K](&expr, .expected_ty = :Some ty)
    ),
}
