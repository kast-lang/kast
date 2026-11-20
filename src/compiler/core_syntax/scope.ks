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
        with Scope.Context = Scope.new(
            .parent = :Some (@current Scope.Context),
            .recursive = false,
        );
        let body_span = body^.span;
        let body = compile[AnyExpr](body, .expected_ty);
        {
            .ty = body.ty,
            .shape = match body.shape with (
                | :Expr shape => :Expr :Scope Rc.new({
                    .shape,
                    .ty = body.ty,
                    .span = body_span,
                })
                | :Place shape => :Place :Scope Rc.new({
                    .shape,
                    .ty = body.ty,
                    .span = body_span,
                })
            ),
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

const impl_for = (K :: Type) -> std.Ast => `(
    (
        ast :: &Ast.t,
        .root :: &Ast.Group,
        .expected_ty :: Option.t[Ty],
    ) -> K => (
        let body = root
            |> AstHelpers.expect_single_child(:None);
        (K as CompileTrait).compile(ast, .body, .expected_ty)
    )
);

(:: CoreSyntax) {
    .name = "scope",
    .compile = {
        .any_expr = include_ast impl_for(AnyExpr),
        .expr = include_ast impl_for(Expr),
        .place_expr = include_ast impl_for(PlaceExpr),
        .ty_expr = include_ast impl_for(TyExpr),
        .pattern = include_ast impl_for(Pattern),
        .assignee = include_ast impl_for(Assignee),
    },
}
