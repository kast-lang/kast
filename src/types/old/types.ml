open Std
open Kast_util
module Ast = Kast_ast
module Syntax = Kast_syntax
module Inference = Kast_inference_base
module Label = Label


and BoolVar : Inference.Var.S = Inference.Var.Make (Bool)

and VarScope : Inference.Scope = struct
  type t = InterpreterScope.t option

  let root () = None

  let common a b =
    match (a, b) with
    | None, None | Some _, None | None, Some _ -> None
    | Some a, Some b ->
        (* TODO calculate LCA *)
        failwith __LOC__
end


and NameVar : Inference.Var.S = Inference.Var.Make (NameShape)

and Name : sig
  type t = { var : NameVar.t } [@@deriving eq, ord]
end = struct
  type t = { var : NameVar.t } [@@deriving eq, ord]
end

and OptionalNameShape : sig
  type t = NameShape.t option
end = struct
  type t = NameShape.t option
end

and OptionalNameShapeVar : Inference.Var.S =
  Inference.Var.Make (OptionalNameShape)

and OptionalName : sig
  type t = { var : OptionalNameShapeVar.t }
end = struct
  type t = { var : OptionalNameShapeVar.t }
end

and Compiled : sig
  type _ kind =
    | Assignee : AssigneeExpr.t kind
    | Expr : Expr.t kind
    | PlaceExpr : PlaceExpr.t kind
    | TyExpr : TyExpr.t kind
    | Pattern : Pattern.t kind

  type t = Compiled : 'a. 'a kind * 'a -> t
end = struct
  type _ kind =
    | Assignee : AssigneeExpr.t kind
    | Expr : Expr.t kind
    | PlaceExpr : PlaceExpr.t kind
    | TyExpr : TyExpr.t kind
    | Pattern : Pattern.t kind

  type t = Compiled : 'a. 'a kind * 'a -> t
end

and ValueMap : sig
end = struct
end