open Std
open Kast_util
open Kast_types
open Types

module Impl = struct
  let rec sub_value ~(state : interpreter_state) (value : value) : value =
    sub_value_shape ~state value.shape

  and sub_value_shape ~(state : interpreter_state) (value : value_shape) : value
      =
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
    | V_Variant { label; data; ty } ->
        V_Variant
          {
            label;
            data = data |> Option.map (sub_value ~state);
            ty = sub_ty ~state ty;
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
    if
      RecurseCache.get ()
      |> RecurseCache.visit (Inference.Var.recurse_id ty.var)
    then
      match ty.var |> Inference.Var.inferred_opt with
      | None -> ty
      | Some shape -> sub_ty_shape ~state shape
    else ty

  and sub_ty_shape ~(state : interpreter_state) (ty : ty_shape) : ty =
    let span = Span.fake "sub_ty_shape" in
    match ty with
    | T_Unit | T_Bool | T_Int32 | T_String | T_Char | T_Target | T_ContextTy
    | T_CompilerScope | T_Error | T_Ast | T_Ty ->
        ty |> Ty.inferred ~span
    | T_Tuple { tuple } ->
        T_Tuple { tuple = tuple |> Tuple.map (sub_ty_tuple_field ~state) }
        |> Ty.inferred ~span
    | T_Variant { variants } ->
        T_Variant
          {
            variants =
              sub_row ~state
                (fun ~state ({ data } : ty_variant_data) : ty_variant_data ->
                  { data = data |> Option.map (sub_ty ~state) })
                variants;
          }
        |> Ty.inferred ~span
    | T_Fn ty -> T_Fn (sub_ty_fn ~state ty) |> Ty.inferred ~span
    | T_Generic { def = _ } -> ty |> Ty.inferred ~span
    | T_UnwindToken { result } ->
        T_UnwindToken { result = sub_ty ~state result } |> Ty.inferred ~span
    | T_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None -> ty |> Ty.inferred ~span
        | Some local -> (
            match local.value |> Value.expect_ty with
            | Some ty -> ty
            | None ->
                Error.error
                  (Span.fake "<sub_ty_shape>")
                  "substituted type binding with non-type";
                T_Error |> Ty.inferred ~span))

  and sub_ty_tuple_field ~(state : interpreter_state)
      ({ label; ty } : ty_tuple_field) : ty_tuple_field =
    { label; ty = sub_ty ~state ty }

  and sub_ty_fn ~state ({ arg; result } : ty_fn) : ty_fn =
    { arg = sub_ty ~state arg; result = sub_ty ~state result }

  and sub_row :
      'a.
      state:interpreter_state ->
      (state:interpreter_state -> 'a -> 'a) ->
      'a Row.t ->
      'a Row.t =
   fun ~state sub_value_impl row ->
    let span = row.var |> Inference.Var.spans |> SpanSet.min_elt in
    match row.var |> Inference.Var.inferred_opt with
    | Some shape -> (
        match shape with
        | R_Empty -> row
        | R_Cons { label; value; rest } ->
            Row.inferred ~span
            <| R_Cons
                 {
                   label;
                   value = sub_value_impl ~state value;
                   rest = sub_row ~state sub_value_impl rest;
                 })
    | None -> row
end

let with_cache f =
 fun ~state value ->
  RecurseCache.with_cache (RecurseCache.create ()) (fun () -> f ~state value)

let sub_value = with_cache Impl.sub_value
let sub_ty = with_cache Impl.sub_ty
