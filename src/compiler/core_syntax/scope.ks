use (import "./_common.ks").*;
use (import "../scope.ks").*;

const CompileTrait = [Self] newtype {
    .compile :: (
        &Ast.t,
        .body :: &Ast.t,
        .expected_ty :: Option.t[Ty],
    ) -> Self,
};

impl AnyExpr as CompileTrait = {
    .compile = (
        ast :: &Ast.t,
        .body :: &Ast.t,
        .expected_ty :: Option.t[Ty],
    ) => (
        with Scope.Context = Scope.new(.parent = :Some &(@current Scope.Context));
        let body = compile[Expr](body, .expected_ty);
        {
            .shape = :Expr :Scope body,
            .ty = body.ty,
        }
    ),
};

impl Expr as CompileTrait = {
    .compile = (...args) => any_expr_to_expr(
        (AnyExpr as CompileTrait).compile(...args),
        .span = args.0^.span,
    ),
};
impl PlaceExpr as CompileTrait = {
    .compile = (...args) => any_expr_to_place_expr(
        (AnyExpr as CompileTrait).compile(...args),
        .span = args.0^.span,
    ),
};
impl TyExpr as CompileTrait = {
    .compile = (
        ast :: &Ast.t,
        .body :: &Ast.t,
        .expected_ty :: Option.t[Ty],
    ) => (
        compile[TyExpr](body, .expected_ty)
    ),
};

impl Assignee as CompileTrait = {
    .compile = (
        ast :: &Ast.t,
        .body :: &Ast.t,
        .expected_ty :: Option.t[Ty],
    ) => (
        compile[Assignee](body, .expected_ty)
    ),
};

impl Pattern as CompileTrait = {
    .compile = (
        ast :: &Ast.t,
        .body :: &Ast.t,
        .expected_ty :: Option.t[Ty],
    ) => (
        compile[Pattern](body, .expected_ty)
    ),
};

(:: CoreSyntax) {
    .name = "scope",
    .compile = [K] (
        ast :: &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> K => (
        let body = root^
            |> AstHelpers.expect_single_child(:None);
        (K as CompileTrait).compile(ast, .body = &body, .expected_ty)
    ),
}
