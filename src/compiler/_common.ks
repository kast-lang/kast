module:

use (import "../id.ks").*;
use (import "../tuple.ks").*;
use (import "../token.ks").*;
use (import "../ast.ks").*;
use (import "../span.ks").*;
use (import "../rc_str.ks").*;
use (import "../syntax_rule.ks").*;
use (import "../diagnostic.ks").*;
use (import "../output.ks").*;
use (import "../hir/_lib.ks").*;
use (import "../interpreter/_lib.ks").*;
use (import "../mini/ast_helpers.ks").*;
use std.collections.OrdMap;
use std.rc.Rc;

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
        | :Place shape => Rc.new({ .shape, .ty, .span })
        | :Expr shape => Rc.new({ .shape = :Temp Rc.new({ .shape, .ty, .span }), .ty, .span })
    )
);

const any_expr_to_expr = (expr :: AnyExpr, .span :: Span) -> Expr => (
    let ty = expr.ty;
    match expr.shape with (
        | :Expr shape => Rc.new({ .shape, .ty, .span })
        | :Place shape => Rc.new({ .shape = :Read Rc.new({ .shape, .ty, .span }), .ty, .span })
    )
);

const any_expr_to_type_expr = (expr :: AnyExpr, .span :: Span) -> TyExpr => (
    Rc.new({
        .shape = :Expr any_expr_to_expr(expr, .span),
        .span,
    })
);

const type_expr_to_expr = (expr :: TyExpr) -> Expr => (
    Rc.new({
        .shape = :Type expr,
        .ty = Ty.TYPE,
        .span = expr^.span,
    })
);

const CompileSpecific = [K] type ((&Ast.t, .expected_ty :: Option.t[Ty]) -> K);

const CompilerContextT = newtype {
    .inject_assignee_bindings :: &Assignee -> (),
    .compile :: {
        .any_expr :: CompileSpecific[AnyExpr],
        .expr :: CompileSpecific[Expr],
        .place_expr :: CompileSpecific[PlaceExpr],
        .ty_expr :: CompileSpecific[TyExpr],
        .assignee :: CompileSpecific[Assignee],
        .pattern :: CompileSpecific[Pattern],
    },
    .impl_syntax :: (
        .rule :: &Rc.t[SyntaxRule.t],
        .pattern_root :: &Ast.Group,
        .bindings :: OrdMap.t[RcStr, Binding],
        .@"impl" :: Expr,
    ) -> (),
};

const CompilerContext = @context CompilerContextT;

const InMut = @context Bool;

const CompileImpl = (
    module:

    const Compilable = [Self] CompileSpecific[Self];

    impl AnyExpr as Compilable = ((...args) => (
        (@current CompilerContext).compile.any_expr(...args)
    ));
    impl Expr as Compilable = ((...args) => (
        (@current CompilerContext).compile.expr(...args)
    ));
    impl PlaceExpr as Compilable = ((...args) => (
        (@current CompilerContext).compile.place_expr(...args)
    ));
    impl TyExpr as Compilable = ((...args) => (
        (@current CompilerContext).compile.ty_expr(...args)
    ));
    impl Assignee as Compilable = ((...args) => (
        (@current CompilerContext).compile.assignee(...args)
    ));
    impl Pattern as Compilable = ((...args) => (
        (@current CompilerContext).compile.pattern(...args)
    ));
);

const compile = [K] (ast :: &Ast.t, .expected_ty :: Option.t[Ty]) -> K => (
    (K as CompileImpl.Compilable)(ast, .expected_ty)
);

const compile_type_expr = (ast :: &Ast.t) -> TyExpr => (
    compile[TyExpr](ast, .expected_ty = :Some Ty.TYPE)
);

const eval_ast_as_type = (ast :: &Ast.t) -> Ty => (
    let expr = compile_type_expr(ast);
    Interpreter.eval_type(&expr)
);
