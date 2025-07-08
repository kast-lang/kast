open Std
open Kast_util
open Kast_types
open Error
module Inference = Kast_inference

let init_expr : ?evaled_exprs:expr list -> span -> Expr.Shape.t -> expr =
 fun ?(evaled_exprs = []) span shape ->
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
      | E_Native _ -> Ty.new_not_inferred ()
      | E_Module { def } ->
          def.data.ty
          |> Inference.Ty.expect_inferred_as ~span:def.data.span
               (Ty.inferred T_Unit);
          (* TODO actually infer as tuple *)
          Ty.new_not_inferred ()
      | E_Field { obj; field } ->
          let ty = Ty.new_not_inferred () in
          obj.data.ty.var
          |> Inference.Var.once_inferred (fun (obj_shape : Ty.Shape.t) ->
                 let field_ty =
                   match obj_shape with
                   | T_Tuple { tuple } -> (
                       match Tuple.get_named_opt field tuple with
                       | Some ty -> ty
                       | None ->
                           error span "field %a is not there"
                             String.print_maybe_escaped field;
                           Ty.new_not_inferred ())
                   | other ->
                       error obj.data.span "%a doesnt have fields"
                         Ty.Shape.print other;
                       Ty.new_not_inferred ()
                 in
                 ty |> Inference.Ty.expect_inferred_as ~span field_ty);
          ty
      | E_UseDotStar { used = _; bindings = _ } -> Ty.inferred T_Unit
      | E_If { cond; then_case; else_case } ->
          cond.data.ty
          |> Inference.Ty.expect_inferred_as ~span:cond.data.span
               (Ty.inferred T_Bool);
          let ty = Ty.new_not_inferred () in
          then_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:then_case.data.span ty;
          else_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:else_case.data.span ty;
          ty
      | E_QuoteAst _ ->
          (* TODO assert all children are ast *)
          Ty.inferred T_Ast
      | E_Error -> Ty.new_not_inferred ()
    in
    { shape; data = { span; ty; ty_ascription = None; evaled_exprs } }
  with exc ->
    Log.error "while initializing expr at %a" Span.print span;
    raise exc

let init_assignee :
    ?evaled_exprs:expr list -> span -> Expr.Assignee.Shape.t -> Expr.assignee =
 fun ?(evaled_exprs = []) span shape ->
  try
    let ty =
      match shape with
      | A_Placeholder -> Ty.new_not_inferred ()
      | A_Unit -> Ty.inferred T_Unit
      | A_Binding binding -> binding.ty
      | A_Let pattern -> pattern.data.ty
      | A_Error -> Ty.new_not_inferred ()
    in
    { shape; data = { span; ty; ty_ascription = None; evaled_exprs } }
  with exc ->
    Log.error "while initializing assignee expr at %a" Span.print span;
    raise exc

let init_pattern : ?evaled_exprs:expr list -> span -> Pattern.Shape.t -> pattern
    =
 fun ?(evaled_exprs = []) span shape ->
  try
    let ty =
      match shape with
      | P_Placeholder -> Ty.new_not_inferred ()
      | P_Unit -> Ty.inferred T_Unit
      | P_Binding binding -> binding.ty
      | P_Tuple { tuple } ->
          Ty.inferred
            (T_Tuple
               {
                 tuple =
                   tuple
                   |> Tuple.map (fun (field_pattern : pattern) ->
                          field_pattern.data.ty);
               })
      | P_Error -> Ty.new_not_inferred ()
    in
    { shape; data = { span; ty; ty_ascription = None; evaled_exprs } }
  with exc ->
    Log.error "while initializing pattern at %a" Span.print span;
    raise exc

let init_ty_expr : ?evaled_exprs:expr list -> span -> Expr.Ty.Shape.t -> Expr.ty
    =
  let type_ty = Ty.inferred T_Ty in
  fun ?(evaled_exprs = []) span shape ->
    try
      (match shape with
      | TE_Unit -> ()
      | TE_Fn { arg; result } ->
          let _ : Expr.ty = arg in
          let _ : Expr.ty = result in
          ()
      | TE_Expr expr ->
          expr.data.ty
          |> Inference.Ty.expect_inferred_as ~span:expr.data.span type_ty
      | TE_Error -> ());
      {
        shape;
        data = { span; ty = type_ty; ty_ascription = None; evaled_exprs };
      }
    with exc ->
      Log.error "while initializing type expr at %a" Span.print span;
      raise exc

let init_error : 'a. span -> 'a Compiler.compiled_kind -> 'a =
 fun (type a) span (kind : a Compiler.compiled_kind) : a ->
  match kind with
  | Expr -> E_Error |> init_expr span
  | Pattern -> P_Error |> init_pattern span
  | Assignee -> A_Error |> init_assignee span
  | TyExpr -> TE_Error |> init_ty_expr span
