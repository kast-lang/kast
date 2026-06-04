module:

use (import "../id.ks").*;
use (import "../token.ks").*;
use (import "../ast.ks").*;
use (import "../span.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../hir/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;
use std.collections.OrdMap;

const AnyExprShape = newtype (
    | :Expr ExprShape
    | :Place PlaceExprShape
);

const AnyExpr = newtype {
    .shape :: AnyExprShape,
    .ty :: Ty,
};

const any_expr_to_place_expr = (expr :: AnyExpr, .span :: Span) -> PlaceExpr => (
    let ty = expr.ty;
    match expr.shape with (
        | :Place shape => { .shape, .ty, .span }
        | :Expr shape => { .shape = :Temp { .shape, .ty, .span }, .ty, .span }
    )
);

const any_expr_to_expr = (expr :: AnyExpr, .span :: Span) -> Expr => (
    let ty = expr.ty;
    match expr.shape with (
        | :Expr shape => { .shape, .ty, .span }
        | :Place shape => { .shape = :Read { .shape, .ty, .span }, .ty, .span }
    )
);

const CompilerContextT = newtype {
    .compile :: [K] (&Ast.t, .expected_ty :: Option.t[Ty]) -> K,
};

const CompilerContext = @context CompilerContextT;

const compile = [K] (ast :: &Ast.t, .expected_ty :: Option.t[Ty]) -> K => (
    (@current CompilerContext).compile[K](ast, .expected_ty)
);

const compile_type_expr = (ast :: &Ast.t) -> TyExpr => (
    compile[TyExpr](ast, .expected_ty = :Some Ty.TYPE)
);
