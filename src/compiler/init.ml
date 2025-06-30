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
    | E_Then { a; b } -> b.data.ty
    | E_Stmt { expr } -> Ty.inferred T_Unit
    | E_Scope { expr } -> expr.data.ty
    | E_Fn { arg; body } ->
        Ty.inferred <| T_Fn { arg = arg.data.ty; result = body.data.ty }
    | E_Tuple { tuple } ->
        Ty.inferred
        <| T_Tuple
             {
               tuple = tuple |> Tuple.map (fun (field : expr) -> field.data.ty);
             }
    | E_Apply { f; arg } ->
        let f_arg = Ty.new_not_inferred () in
        let f_result = Ty.new_not_inferred () in
        f.data.ty
        |> Inference.Ty.expect_inferred_as
             (Ty.inferred <| T_Fn { arg = f_arg; result = f_result });
        arg.data.ty |> Inference.Ty.expect_inferred_as f_arg;
        f_result
    | E_Assign { assignee; value } ->
        assignee.data.ty |> Inference.Ty.expect_inferred_as value.data.ty;
        Ty.inferred T_Unit
  in
  { shape; data = { span; ty; ty_ascription = None } }

let init_assignee : span -> Expr.Assignee.Shape.t -> Expr.assignee =
 fun span shape ->
  let ty =
    match shape with
    | A_Placeholder -> Ty.new_not_inferred ()
    | A_Unit -> Ty.inferred T_Unit
    | A_Binding binding -> binding.ty
    | A_Let pattern -> pattern.data.ty
  in
  { shape; data = { span; ty; ty_ascription = None } }

let init_pattern : span -> Pattern.Shape.t -> pattern =
 fun span shape ->
  let ty =
    match shape with
    | P_Placeholder -> Ty.new_not_inferred ()
    | P_Unit -> Ty.inferred T_Unit
    | P_Binding binding -> binding.ty
  in
  { shape; data = { span; ty; ty_ascription = None } }
