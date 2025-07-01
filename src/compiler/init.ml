open Std
open Kast_util
open Kast_types
module Inference = Kast_inference

let init_expr : span -> Expr.Shape.t -> expr =
 fun span shape ->
  try
    let ty =
      match shape with
      | E_Constant value -> Value.ty_of value
      | E_Binding binding -> binding.ty
      | E_Then { a; b } -> b.data.ty
      | E_Stmt { expr } -> Ty.inferred T_Unit
      | E_Scope { expr } -> expr.data.ty
      | E_Fn { arg; body; evaled_result = _ } ->
          Ty.inferred <| T_Fn { arg = arg.data.ty; result = body.data.ty }
      | E_Tuple { tuple } ->
          Ty.inferred
          <| T_Tuple
               {
                 tuple =
                   tuple |> Tuple.map (fun (field : expr) -> field.data.ty);
               }
      | E_Apply { f; arg } ->
          let f_arg = Ty.new_not_inferred () in
          let f_result = Ty.new_not_inferred () in
          f.data.ty
          |> Inference.Ty.expect_inferred_as ~span:f.data.span
               (Ty.inferred <| T_Fn { arg = f_arg; result = f_result });
          arg.data.ty
          |> Inference.Ty.expect_inferred_as ~span:arg.data.span f_arg;
          f_result
      | E_Assign { assignee; value } ->
          value.data.ty
          |> Inference.Ty.expect_inferred_as ~span:value.data.span
               assignee.data.ty;
          Ty.inferred T_Unit
      | E_Ty _ -> Ty.inferred T_Ty
    in
    { shape; data = { span; ty; ty_ascription = None } }
  with exc ->
    Log.error "while initializing expr at %a" Span.print span;
    raise exc

let init_assignee : span -> Expr.Assignee.Shape.t -> Expr.assignee =
 fun span shape ->
  try
    let ty =
      match shape with
      | A_Placeholder -> Ty.new_not_inferred ()
      | A_Unit -> Ty.inferred T_Unit
      | A_Binding binding -> binding.ty
      | A_Let pattern -> pattern.data.ty
    in
    { shape; data = { span; ty; ty_ascription = None } }
  with exc ->
    Log.error "while initializing assignee expr at %a" Span.print span;
    raise exc

let init_pattern : span -> Pattern.Shape.t -> pattern =
 fun span shape ->
  try
    let ty =
      match shape with
      | P_Placeholder -> Ty.new_not_inferred ()
      | P_Unit -> Ty.inferred T_Unit
      | P_Binding binding -> binding.ty
    in
    { shape; data = { span; ty; ty_ascription = None } }
  with exc ->
    Log.error "while initializing pattern at %a" Span.print span;
    raise exc

let init_ty_expr : span -> Expr.Ty.Shape.t -> Expr.ty =
  let type_ty = Ty.inferred T_Ty in
  fun span shape ->
    try
      (match shape with
      | TE_Unit -> ()
      | TE_Expr expr ->
          expr.data.ty
          |> Inference.Ty.expect_inferred_as ~span:expr.data.span type_ty);
      { shape; data = { span; ty = type_ty; ty_ascription = None } }
    with exc ->
      Log.error "while initializing type expr at %a" Span.print span;
      raise exc
