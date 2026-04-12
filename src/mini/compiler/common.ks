module:

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

use (import "../ir/_lib.ks").*;
use (import "../ast_helpers.ks").*;

const TypeKind = newtype (
    | :Opaque
    | :Enum
    | :Struct
    | :Union
    | :Alias
);

const Ty = Ir.Type;

const TemplateArgs = newtype {
    .args :: ArrayList.t[Ir.Type],
    .by_name :: OrdMap.t[String, Ir.Type],
};

const Instantiation = newtype {
    .name :: String,
};

const Template = newtype {
    .arg_names :: ArrayList.t[String],
    .def :: Ast.t,
    .instantiations :: OrdMap.t[TemplateArgs, Instantiation],
};

const ParsedExprShape = newtype (
    | :Expr Ir.ExprShape
    | :Place Ir.PlaceExprShape
);

const ParsedExpr = newtype {
    .shape :: ParsedExprShape,
    .ty :: Ir.Type,
};

const ConstDeclaration = newtype {
    .ty :: Ty,
    .value :: Ast.t,
};

const TopLevelItemDef = newtype {
    .name :: String,
    .span :: Span,
    .ast :: Ast.t,
    .setup_contexts :: Option.t[type ((() -> ()) -> ())],
};

const TopLevelDecl = newtype (
    | :Template
    | :Type
    | :Const ConstDeclaration
    | :Fn Ir.FnType
    | :Context
);

const TopLevelImpl = newtype (
    | :Template Template
    | :Type Ir.TypeDef
    | :Const Ir.Expr
    | :Fn Ir.FnDef
    | :Context Ty
);

const CompilerT = newtype {
    .program :: Ir.Program,
    .parse_type :: Ast.t -> Ty,
    .register_type_def :: (String, Ir.TypeDef) -> (),
    .find_ident_ty :: (String, .span :: Span) -> Ty,
    .lookup_type :: (String, .span :: Span) -> Ty,
    .add_toplevel_item :: TopLevelItemDef -> (),
    .get_toplevel_decl :: String -> Option.t[TopLevelDecl],
    .get_toplevel_impl :: String -> Option.t[TopLevelImpl],
    # TODO Ty = { .alias_resolved :: Ir.Ty, .local_name :: Ir.Ty }
    .resolve_type_aliases :: Ty -> Ty,
};

const Compiler = @context CompilerT;

const single_element_list = [T] (x :: T) -> ArrayList.t[T] => (
    let mut result = ArrayList.new();
    &mut result |> ArrayList.push_back(x);
    result
);
