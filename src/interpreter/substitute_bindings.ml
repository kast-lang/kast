open Std
open Kast_util
open Kast_types
open Types

module Impl = struct
  let rec sub_value ~(state : interpreter_state) (value : value) : value option
      =
    sub_value_shape ~state value.shape

  and sub_value_shape ~(state : interpreter_state) (value : value_shape) :
      value option =
    let shaped shape : value = { shape } in
    match value with
    | V_Unit | V_Bool _ | V_Int32 _ | V_Char _ | V_String _ | V_Ast _
    | V_CompilerScope _ | V_Error ->
        None
    | V_Tuple { tuple } ->
        let sub_happened = ref false in
        let result =
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
                       let value =
                         match sub_value ~state value with
                         | Some new_value ->
                             sub_happened := true;
                             new_value
                         | None -> value
                       in
                       let ty_field =
                         match sub_ty_tuple_field ~state ty_field with
                         | Some new_field ->
                             sub_happened := true;
                             new_field
                         | None -> ty_field
                       in
                       { value; span; ty_field });
            }
        in
        if !sub_happened then Some (result |> shaped) else None
    | V_Variant { label; data; ty } ->
        let sub_happened = ref false in
        let result =
          V_Variant
            {
              label;
              data =
                data
                |> Option.map (fun value ->
                    match sub_value ~state value with
                    | Some new_value ->
                        sub_happened := true;
                        new_value
                    | None -> value);
              ty =
                (match sub_ty ~state ty with
                | Some new_ty ->
                    sub_happened := true;
                    new_ty
                | None -> ty);
            }
        in
        if !sub_happened then Some (result |> shaped) else None
    | V_Ty ty ->
        sub_ty ~state ty |> Option.map (fun new_ty -> V_Ty new_ty |> shaped)
    | V_Fn { ty; fn } ->
        sub_ty_fn ~state ty |> Option.map (fun ty -> V_Fn { ty; fn } |> shaped)
    | V_Generic { fn = _ } -> None
    | V_NativeFn { name; ty; impl } ->
        sub_ty_fn ~state ty
        |> Option.map (fun ty -> V_NativeFn { name; ty; impl } |> shaped)
    | V_UnwindToken { id; result_ty } ->
        result_ty |> sub_ty ~state
        |> Option.map (fun result_ty ->
            V_UnwindToken { id; result_ty } |> shaped)
    | V_Target _ -> None
    | V_ContextTy { id; ty } ->
        ty |> sub_ty ~state
        |> Option.map (fun ty -> V_ContextTy { id; ty } |> shaped)
    | V_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None -> None
        | Some local -> Some local.value)

  and sub_ty ~(state : interpreter_state) (ty : ty) : ty option =
    if
      RecurseCache.get ()
      |> RecurseCache.visit (Inference.Var.recurse_id ty.var)
    then
      match ty.var |> Inference.Var.inferred_opt with
      | None -> None
      | Some shape -> sub_ty_shape ~state shape
    else None (* TODO use previous substitution *)

  and sub_ty_shape ~(state : interpreter_state) (shape : ty_shape) : ty option =
    let span = Span.fake "sub_ty_shape" in
    match shape with
    | T_Unit | T_Bool | T_Int32 | T_String | T_Char | T_Target | T_ContextTy
    | T_CompilerScope | T_Error | T_Ast | T_Ty ->
        None
    | T_Tuple { tuple } ->
        let sub_happened = ref false in
        let result =
          T_Tuple
            {
              tuple =
                tuple
                |> Tuple.map (fun field ->
                    match sub_ty_tuple_field ~state field with
                    | Some sub_field ->
                        sub_happened := true;
                        sub_field
                    | None -> field);
            }
        in
        if !sub_happened then Some (result |> Ty.inferred ~span) else None
    | T_Variant { variants } ->
        variants
        |> sub_row ~state
             (fun
               ~state ({ data } : ty_variant_data) : ty_variant_data option ->
               data
               |> Option.and_then (sub_ty ~state)
               |> Option.map (fun data -> { data = Some data }))
        |> Option.map (fun new_variants ->
            T_Variant { variants = new_variants } |> Ty.inferred ~span)
    | T_Fn ty ->
        sub_ty_fn ~state ty
        |> Option.map (fun ty -> T_Fn ty |> Ty.inferred ~span)
    | T_Generic { def = _ } -> None
    | T_UnwindToken { result } ->
        sub_ty ~state result
        |> Option.map (fun result ->
            T_UnwindToken { result } |> Ty.inferred ~span)
    | T_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None -> None
        | Some local -> (
            match local.value |> Value.expect_ty with
            | Some ty -> Some ty
            | None ->
                Error.error
                  (Span.fake "<sub_ty_shape>")
                  "substituted type binding with non-type";
                Some (T_Error |> Ty.inferred ~span)))

  and sub_ty_tuple_field ~(state : interpreter_state)
      ({ label; ty } : ty_tuple_field) : ty_tuple_field option =
    sub_ty ~state ty |> Option.map (fun ty -> { label; ty })

  and sub_ty_fn ~state ({ arg; result } : ty_fn) : ty_fn option =
    let sub_happened = ref false in
    let result : ty_fn =
      {
        arg =
          (match sub_ty ~state arg with
          | Some new_arg ->
              sub_happened := true;
              new_arg
          | None -> arg);
        result =
          (match sub_ty ~state result with
          | Some new_result ->
              sub_happened := true;
              new_result
          | None -> result);
      }
    in
    if !sub_happened then Some result else None

  and sub_row :
      'a.
      state:interpreter_state ->
      (state:interpreter_state -> 'a -> 'a option) ->
      'a Row.t ->
      'a Row.t option =
   fun ~state sub_value_impl row ->
    let span = row.var |> Inference.Var.spans |> SpanSet.min_elt in
    match row.var |> Inference.Var.inferred_opt with
    | Some shape -> (
        match shape with
        | R_Empty -> None
        | R_Cons { label; value; rest } ->
            let sub_happened = ref false in
            let result =
              Row.inferred ~span
              <| R_Cons
                   {
                     label;
                     value =
                       (match sub_value_impl ~state value with
                       | Some new_value ->
                           sub_happened := true;
                           new_value
                       | None -> value);
                     rest =
                       (match sub_row ~state sub_value_impl rest with
                       | Some new_rest ->
                           sub_happened := true;
                           new_rest
                       | None -> rest);
                   }
            in
            if !sub_happened then Some result else None)
    | None -> None
end

let with_cache f =
 fun ~state value ->
  RecurseCache.with_cache (RecurseCache.create ()) (fun () -> f ~state value)

let sub_value = with_cache Impl.sub_value
let sub_ty = with_cache Impl.sub_ty
