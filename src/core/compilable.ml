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
  | Expr -> { shape = Expr.Shape.Error; span; ty = Ty.inferred Ty.Shape.Error }
  | TyExpr ->
      { shape = Expr.Ty.Shape.Error; span; ty = Ty.inferred Ty.Shape.Ty }
  | Assignee ->
      { shape = Assignee.Shape.Error; span; ty = Ty.inferred Ty.Shape.Error }
  | Pattern ->
      { shape = Pattern.Shape.Error; span; ty = Ty.inferred Ty.Shape.Error }

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
