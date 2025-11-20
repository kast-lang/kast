open Std
open Kast_util
open Kast_types
open Types

let rec sub_value ~(state : interpreter_state) (value : value) : value =
  sub_value_shape ~state value.shape

and sub_value_shape ~(state : interpreter_state) (value : value_shape) : value =
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
                     value = sub_value ~state value;
                     span;
                     ty_field = sub_ty_tuple_field ~state ty_field;
                   });
        }
      |> shaped
  | V_Ty ty -> V_Ty (sub_ty ~state ty) |> shaped
  | V_Fn { ty; fn } -> V_Fn { ty = sub_ty_fn ~state ty; fn } |> shaped
  | V_Generic { fn = _ } -> value |> shaped
  | V_NativeFn { name; ty; impl } ->
      V_NativeFn { name; ty = sub_ty_fn ~state ty; impl } |> shaped
  | V_UnwindToken { id; result_ty } ->
      V_UnwindToken { id; result_ty = sub_ty ~state result_ty } |> shaped
  | V_Target _ -> value |> shaped
  | V_ContextTy { id; ty } ->
      V_ContextTy { id; ty = sub_ty ~state ty } |> shaped
  | V_Binding binding -> (
      match Scope.find_local_opt binding.name state.scope with
      | None -> value |> shaped
      | Some local -> local.value)

and sub_ty ~(state : interpreter_state) (ty : ty) : ty =
  match ty.var |> Inference.Var.inferred_opt with
  | None -> ty
  | Some shape -> sub_ty_shape ~state shape

and sub_ty_shape ~(state : interpreter_state) (ty : ty_shape) : ty =
  match ty with
  | T_Unit | T_Bool | T_Int32 | T_String | T_Char | T_Target | T_ContextTy
  | T_CompilerScope | T_Error | T_Ast | T_Ty ->
      ty |> Ty.inferred
  | T_Tuple { tuple } ->
      T_Tuple { tuple = tuple |> Tuple.map (sub_ty_tuple_field ~state) }
      |> Ty.inferred
  | T_Fn ty -> T_Fn (sub_ty_fn ~state ty) |> Ty.inferred
  | T_Generic { fn = _ } -> ty |> Ty.inferred
  | T_UnwindToken { result } ->
      T_UnwindToken { result = sub_ty ~state result } |> Ty.inferred
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

and sub_ty_tuple_field ~(state : interpreter_state)
    ({ label; ty } : ty_tuple_field) : ty_tuple_field =
  { label; ty = sub_ty ~state ty }

and sub_ty_fn ~state ({ arg; result } : ty_fn) : ty_fn =
  { arg = sub_ty ~state arg; result = sub_ty ~state result }
