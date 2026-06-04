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
        let ty = compile_type_expr(&ty);
        let ty = Interpreter.eval_type(&ty);
        compile[K](&expr, .expected_ty = :Some ty)
    ),
}
