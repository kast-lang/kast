open Std
open Kast_util
open Kast_types

type _ compiled_kind =
  | Assignee : Expr.assignee compiled_kind
  | Expr : expr compiled_kind
  | PlaceExpr : Expr.Place.t compiled_kind
  | TyExpr : Expr.ty compiled_kind
  | Pattern : pattern compiled_kind

module CompilerEffect = struct
  type 'a file_included = {
    root : Uri.t;
    uri : Uri.t;
    parsed : Kast_parser.result;
    kind : 'a compiled_kind;
    compiled : 'a;
  }

  type file_imported = {
    uri : Uri.t;
    parsed : Kast_parser.result;
    compiled : expr;
    value : value;
  }

  type _ Effect.t += FileIncluded : 'a. 'a file_included -> unit Effect.t
  type _ Effect.t += FileImported : file_imported -> unit Effect.t
  type _ Effect.t += FileStartedProcessing : Uri.t -> unit Effect.t
  type _ Effect.t += FindStd : Uri.t Effect.t
end
