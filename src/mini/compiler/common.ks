module:

use (import "../../util.ks").*;
use (import "../../log.ks").*;
use (import "../../output.ks").*;
use (import "../../diagnostic.ks").*;
use (import "../../tuple.ks").*;
use (import "../../position.ks").*;
use (import "../../span.ks").*;
use (import "../../source.ks").*;
use (import "../../source_path.ks").*;
use (import "../../token.ks").*;
use (import "../../token_stream.ks").*;
use (import "../../lexer/_lib.ks").*;
use (import "../../syntax_ruleset.ks").*;
use (import "../../syntax_parser.ks").*;
use (import "../../parser.ks").*;
use (import "../../ast.ks").*;
use (import "../../highlight.ks").*;
use (import "../../queue.ks").*;
use std.collections.OrdMap;
use std.collections.OrdSet;

use (import "../interpreter/_lib.ks").*;
use (import "../ir/_lib.ks").*;
use Ir.Types.*;
use (import "../ast_helpers.ks").*;
# TODO allow custom targets
const CompilationTarget = newtype (
    | :C
    | :JavaScript
);

const Ty = Ir.Type;

const ParsedExprShape = newtype (
    | :Expr Ir.ExprShape
    | :Place Ir.PlaceExprShape
);

const ParsedExpr = newtype {
    .shape :: ParsedExprShape,
    .ty :: Ir.Type,
};

const CompilerT = newtype {
    .parse_expr :: (Option.t[Ty], Ast.t) -> Ir.Expr,
    .parse_type :: Ast.t -> Ty,
    .find_ident_ty :: (String, .span :: Span) -> Ty,
    .lookup_type :: (String, .span :: Span) -> Ty,
};

const Compiler = @context CompilerT;

const eval_ast = (expected_ty :: Option.t[Ir.Type], ast :: Ast.t) -> Ir.Value => (
    let expr = (@current Compiler).parse_expr(expected_ty, ast);
    Interpreter.eval(&expr)
);