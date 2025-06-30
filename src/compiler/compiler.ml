open Std
open Kast_util
open Kast_types
module Ast = Kast_ast

type _ compiled_kind =
  | Assignee : Expr.assignee compiled_kind
  | Expr : expr compiled_kind
  | Pattern : pattern compiled_kind

module type S = sig
  val state : State.t
  val compile : 'a. ?state:State.t -> 'a compiled_kind -> Ast.t -> 'a
end

type compiler = { compile : 'a. 'a compiled_kind -> Ast.t -> 'a }

let get_span : 'a. 'a compiled_kind -> 'a -> span =
 fun (type a) (kind : a compiled_kind) (compiled : a) : span ->
  match kind with
  | Expr -> compiled.span
  | Assignee -> compiled.span
  | Pattern -> compiled.span
