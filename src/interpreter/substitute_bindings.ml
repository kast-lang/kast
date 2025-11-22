open Std
open Kast_util
open Kast_types
open Types

let rec sub_value_impl ~cache ~(state : interpreter_state) (value : value) :
    value =
  sub_value_shape_impl ~cache ~state value.shape

and sub_value_shape_impl ~cache ~(state : interpreter_state)
    (value : value_shape) : value =
  let shaped shape : value = { shape } in
  match value with
  | V_Unit | V_Bool _ | V_Int32 _ | V_Char _ | V_String _ | V_Ast _
  | V_CompilerScope _ | V_Error ->
      value |> shaped
  | V_Tuple { tuple } ->
      V_Tuple
        {
          tuple =
            tuple
            |> Tuple.map
                 (fun
                   ({ value; span; ty_field } : value_tuple_field)
                   :
                   value_tuple_field
                 ->
                   {
                     value = sub_value_impl ~cache ~state value;
                     span;
                     ty_field = sub_ty_tuple_field_impl ~cache ~state ty_field;
                   });
        }
      |> shaped
  | V_Variant { label; data; ty } ->
      V_Variant
        {
          label;
          data = data |> Option.map (sub_value_impl ~cache ~state);
          ty = sub_ty_impl ~cache ~state ty;
        }
      |> shaped
  | V_Ty ty -> V_Ty (sub_ty_impl ~cache ~state ty) |> shaped
  | V_Fn { ty; fn } ->
      V_Fn { ty = sub_ty_fn_impl ~cache ~state ty; fn } |> shaped
  | V_Generic { fn = _ } -> value |> shaped
  | V_NativeFn { name; ty; impl } ->
      V_NativeFn { name; ty = sub_ty_fn_impl ~cache ~state ty; impl } |> shaped
  | V_UnwindToken { id; result_ty } ->
      V_UnwindToken { id; result_ty = sub_ty_impl ~cache ~state result_ty }
      |> shaped
  | V_Target _ -> value |> shaped
  | V_ContextTy { id; ty } ->
      V_ContextTy { id; ty = sub_ty_impl ~cache ~state ty } |> shaped
  | V_Binding binding -> (
      match Scope.find_local_opt binding.name state.scope with
      | None -> value |> shaped
      | Some local -> local.value)

and sub_ty_impl ~cache ~(state : interpreter_state) (ty : ty) : ty =
  if cache |> RecurseCache.visit (Inference.Var.recurse_id ty.var) then
    match ty.var |> Inference.Var.inferred_opt with
    | None -> ty
    | Some shape -> sub_ty_shape_impl ~cache ~state shape
  else ty

and sub_ty_shape_impl ~cache ~(state : interpreter_state) (ty : ty_shape) : ty =
  match ty with
  | T_Unit | T_Bool | T_Int32 | T_String | T_Char | T_Target | T_ContextTy
  | T_CompilerScope | T_Error | T_Ast | T_Ty ->
      ty |> Ty.inferred
  | T_Tuple { tuple } ->
      T_Tuple
        { tuple = tuple |> Tuple.map (sub_ty_tuple_field_impl ~cache ~state) }
      |> Ty.inferred
  | T_Variant { variants } ->
      T_Variant
        {
          variants =
            sub_row_impl ~cache ~state
              (fun ~cache ~state ({ data } : ty_variant_data) : ty_variant_data
                 -> { data = data |> Option.map (sub_ty_impl ~cache ~state) })
              variants;
        }
      |> Ty.inferred
  | T_Fn ty -> T_Fn (sub_ty_fn_impl ~cache ~state ty) |> Ty.inferred
  | T_Generic { def = _ } -> ty |> Ty.inferred
  | T_UnwindToken { result } ->
      T_UnwindToken { result = sub_ty_impl ~cache ~state result } |> Ty.inferred
  | T_Binding binding -> (
      match Scope.find_local_opt binding.name state.scope with
      | None -> ty |> Ty.inferred
      | Some local -> (
          match local.value |> Value.expect_ty with
          | Some ty -> ty
          | None ->
              Error.error
                (Span.fake "<sub_ty_shape>")
                "substituted type binding with non-type";
              T_Error |> Ty.inferred))

and sub_ty_tuple_field_impl ~cache ~(state : interpreter_state)
    ({ label; ty } : ty_tuple_field) : ty_tuple_field =
  { label; ty = sub_ty_impl ~cache ~state ty }

and sub_ty_fn_impl ~cache ~state ({ arg; result } : ty_fn) : ty_fn =
  {
    arg = sub_ty_impl ~cache ~state arg;
    result = sub_ty_impl ~cache ~state result;
  }

and sub_row_impl :
    'a.
    cache:RecurseCache.t ->
    state:interpreter_state ->
    (cache:RecurseCache.t -> state:interpreter_state -> 'a -> 'a) ->
    'a Row.t ->
    'a Row.t =
 fun ~cache ~state sub_value_impl row ->
  match row.var |> Inference.Var.inferred_opt with
  | Some shape -> (
      match shape with
      | R_Empty -> row
      | R_Cons { label; value; rest } ->
          Row.inferred
            (R_Cons
               {
                 label;
                 value = sub_value_impl ~cache ~state value;
                 rest = sub_row_impl ~cache ~state sub_value_impl rest;
               }))
  | None -> row

let sub_value ~state value =
  sub_value_impl ~cache:(RecurseCache.create ()) ~state value

let sub_ty ~state ty = sub_ty_impl ~cache:(RecurseCache.create ()) ~state ty
