open Std
open Kast_util
open Kast_types
module Ast = Kast_ast

type _ compiled_kind =
  | Assignee : assignee_expr compiled_kind
  | Expr : expr compiled_kind
  | Pattern : pattern compiled_kind

module type S = sig
  val compile : 'a. 'a compiled_kind -> Ast.t -> 'a
end

type compiler = { compile : 'a. 'a compiled_kind -> Ast.t -> 'a }
