open Std
open Kast_util
open Kast_types
module Ast = Kast_ast
module Inference = Kast_inference

type _ compiled_kind =
  | Assignee : Expr.assignee compiled_kind
  | Expr : expr compiled_kind
  | Pattern : pattern compiled_kind

module type S = sig
  val state : State.t
  val compile : 'a. ?state:State.t -> 'a compiled_kind -> Ast.t -> 'a
end

type compiler = { compile : 'a. 'a compiled_kind -> Ast.t -> 'a }

let get_data : 'a. 'a compiled_kind -> 'a -> ir_data =
 fun (type a) (kind : a compiled_kind) (compiled : a) : ir_data ->
  match kind with
  | Expr -> compiled.data
  | Assignee -> compiled.data
  | Pattern -> compiled.data

let update_data : 'a. 'a compiled_kind -> 'a -> (ir_data -> ir_data) -> 'a =
 fun (type a) (kind : a compiled_kind) (compiled : a) (f : ir_data -> ir_data) :
     a ->
  match kind with
  | Expr -> { compiled with data = f compiled.data }
  | Assignee -> { compiled with data = f compiled.data }
  | Pattern -> { compiled with data = f compiled.data }

let eval_ty (module C : S) (ast : Ast.t) =
  let expected_ty_expr = C.compile Expr ast in
  let expected_ty : ty =
    expected_ty_expr.data.ty
    |> Inference.Ty.expect_inferred_as (Ty.inferred T_Ty);
    let expected_ty =
      Kast_interpreter.eval C.state.interpreter expected_ty_expr
    in
    expected_ty |> Value.expect_ty
  in
  (expected_ty, expected_ty_expr)
