open Std
open Kast_util
open Kast_types
module Inference = Kast_inference

let init_expr : span -> Expr.Shape.t -> expr =
 fun span shape ->
  let ty =
    match shape with
    | E_Constant value -> Value.ty_of value
    | E_Binding binding -> binding.ty
    | E_Then { a; b } -> b.ty
    | E_Stmt { expr } -> Ty.inferred T_Unit
    | E_Scope { expr } -> expr.ty
    | E_Fn { arg; body } ->
        Ty.inferred <| T_Fn { arg = arg.ty; result = body.ty }
    | E_Tuple { tuple } ->
        Ty.inferred
        <| T_Tuple
             { tuple = tuple |> Tuple.map (fun (field : expr) -> field.ty) }
    | E_Apply { f; arg } ->
        let f_arg = Ty.new_not_inferred () in
        let f_result = Ty.new_not_inferred () in
        f.ty
        |> Inference.Ty.expect_inferred_as
             (Ty.inferred <| T_Fn { arg = f_arg; result = f_result });
        arg.ty |> Inference.Ty.expect_inferred_as f_arg;
        f_result
    | E_Assign { assignee; value } ->
        assignee.ty |> Inference.Ty.expect_inferred_as value.ty;
        Ty.inferred T_Unit
  in
  { shape; span; ty }

let init_assignee : span -> Expr.Assignee.Shape.t -> Expr.assignee =
 fun span shape ->
  let ty =
    match shape with
    | A_Placeholder -> Ty.new_not_inferred ()
    | A_Binding binding -> binding.ty
    | A_Let pattern -> pattern.ty
  in
  { shape; span; ty }

let init_pattern : span -> Pattern.Shape.t -> pattern =
 fun span shape ->
  let ty =
    match shape with
    | P_Placeholder -> Ty.new_not_inferred ()
    | P_Binding binding -> binding.ty
  in
  { shape; span; ty }
