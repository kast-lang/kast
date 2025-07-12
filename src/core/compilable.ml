open Std
open Kast_util

type 'a t =
  | Expr : Expr.t t
  | TyExpr : Expr.Ty.t t
  | Assignee : Assignee.t t
  | Pattern : Pattern.t t

let error : 'a. span -> 'a t -> 'a =
 fun (type a) (span : span) (kind : a t) : a ->
  match kind with
  | Expr -> { shape = Expr.Shape.Error; span; ty = Ty.error () }
  | TyExpr ->
      { shape = Expr.Ty.Shape.Error; span; ty = Ty.inferred Ty.Shape.Ty }
  | Assignee -> { shape = Assignee.Shape.Error; span; ty = Ty.error () }
  | Pattern -> { shape = Pattern.Shape.Error; span; ty = Ty.error () }

let unit : 'a. span -> 'a t -> 'a =
 fun (type a) (span : span) (kind : a t) : a ->
  match kind with
  | Expr -> { shape = Expr.Shape.Unit; span; ty = Ty.inferred Ty.Shape.Unit }
  | TyExpr -> { shape = Expr.Ty.Shape.Unit; span; ty = Ty.inferred Ty.Shape.Ty }
  | Assignee ->
      { shape = Assignee.Shape.Unit; span; ty = Ty.inferred Ty.Shape.Unit }
  | Pattern ->
      { shape = Pattern.Shape.Unit; span; ty = Ty.inferred Ty.Shape.Unit }

let print (type a) fmt (kind : a t) =
  let s =
    match kind with
    | Expr -> "expression"
    | TyExpr -> "type expression"
    | Assignee -> "assignee"
    | Pattern -> "pattern"
  in
  fprintf fmt "%s" s

module Data = struct
  type data = {
    span : span;
    ty : Ty.t;
  }

  let get (type a) (kind : a t) (expr : a) : data =
    match kind with
    | Expr -> { span = expr.span; ty = expr.ty }
    | TyExpr -> { span = expr.span; ty = expr.ty }
    | Assignee -> { span = expr.span; ty = expr.ty }
    | Pattern -> { span = expr.span; ty = expr.ty }

  let set (type a) (kind : a t) (expr : a) ({ span; ty } : data) : a =
    match kind with
    | Expr -> { expr with span; ty }
    | TyExpr -> { expr with span; ty }
    | Assignee -> { expr with span; ty }
    | Pattern -> { expr with span; ty }

  let update (type a) (kind : a t) (expr : a) (f : data -> data) : a =
    set kind expr (f (get kind expr))

  type t = data
end
