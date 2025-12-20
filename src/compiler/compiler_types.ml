open Std
open Kast_util
open Kast_types

let get_data : 'a. 'a compiled_kind -> 'a -> ir_data =
 fun (type a) (kind : a compiled_kind) (compiled : a) : ir_data ->
  match kind with
  | Expr -> compiled.data
  | Assignee -> compiled.data
  | TyExpr -> compiled.data
  | Pattern -> compiled.data
  | PlaceExpr -> compiled.data

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
