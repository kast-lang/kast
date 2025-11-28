open Std
open Kast_util
open Kast_types
open Types

module Impl = struct
  type subbed_vars = (id, Obj.t) Hashtbl.t
  type _ Effect.t += GetVars : subbed_vars Effect.t

  let with_subbed (type a) (vars : subbed_vars) (f : unit -> a) : a =
    try f () with effect GetVars, k -> Effect.continue k vars

  let subs_count = ref 0

  let rec sub_value ~(state : interpreter_state) (value : value) : value =
    sub_var ~unite_shape:Inference_impl.unite_value_shape
      ~sub_shape:sub_value_shape ~new_not_inferred:Value.new_not_inferred
      ~get_var:(fun (value : value) -> value.var)
      ~state value

  and sub_value_shape ~(state : interpreter_state) (original_value : value)
      (shape : value_shape) : value =
    let span = Span.fake "<sub_value_shape>" in
    let shaped shape = Value.inferred ~span shape in
    match shape with
    | V_Unit | V_Bool _ | V_Int32 _ | V_Char _ | V_String _ | V_Ast _
    | V_CompilerScope _ | V_Error ->
        original_value
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
            data = data |> Option.map (fun value -> sub_value ~state value);
            ty = sub_ty ~state ty;
          }
        |> shaped
    | V_Ty ty -> V_Ty (sub_ty ~state ty) |> shaped
    | V_Fn { ty; fn } -> V_Fn { ty = sub_ty_fn ~state ty; fn } |> shaped
    | V_Generic { id = _; fn = _ } -> original_value
    | V_NativeFn { id; name; ty; impl } ->
        V_NativeFn { id; name; ty = sub_ty_fn ~state ty; impl } |> shaped
    | V_UnwindToken { id; result_ty } ->
        V_UnwindToken { id; result_ty = sub_ty ~state result_ty } |> shaped
    | V_Target _ -> original_value
    | V_ContextTy { id; ty } ->
        V_ContextTy { id; ty = sub_ty ~state ty } |> shaped
    | V_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None -> original_value
        | Some local ->
            subs_count := !subs_count + 1;
            local.value)

  and sub_ty ~state ty =
    sub_var ~unite_shape:Inference_impl.unite_ty_shape ~sub_shape:sub_ty_shape
      ~new_not_inferred:Ty.new_not_inferred
      ~get_var:(fun (ty : ty) -> ty.var)
      ~state ty

  and sub_ty_shape ~(state : interpreter_state) (original_ty : ty)
      (shape : ty_shape) : ty =
    let span = Span.fake "<sub_ty_shape>" in
    let shaped shape = Ty.inferred ~span shape in
    match shape with
    | T_Unit | T_Bool | T_Int32 | T_String | T_Char | T_Target | T_ContextTy
    | T_CompilerScope | T_Error | T_Ast | T_Ty ->
        original_ty
    | T_Tuple { tuple } ->
        T_Tuple
          {
            tuple =
              tuple |> Tuple.map (fun field -> sub_ty_tuple_field ~state field);
          }
        |> shaped
    | T_Variant { variants } ->
        T_Variant
          {
            variants =
              variants
              |> sub_row ~unite_value:Inference_impl.unite_ty_variant_data
                   ~state
                   ~sub_value:(fun
                       ~state ({ data } : ty_variant_data) : ty_variant_data ->
                     { data = data |> Option.map (sub_ty ~state) });
          }
        |> shaped
    | T_Fn ty -> T_Fn (sub_ty_fn ~state ty) |> shaped
    | T_Generic { def = _ } -> original_ty
    | T_UnwindToken { result } ->
        T_UnwindToken { result = result |> sub_ty ~state } |> shaped
    | T_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None -> original_ty
        | Some local -> (
            subs_count := !subs_count + 1;
            match local.value |> Value.expect_ty with
            | Some ty -> ty
            | None ->
                Error.error span "substituted type binding with non-type";
                T_Error |> shaped))

  and sub_ty_tuple_field ~(state : interpreter_state)
      ({ label; ty } : ty_tuple_field) : ty_tuple_field =
    { label; ty = sub_ty ~state ty }

  and sub_ty_fn ~state ({ arg; result } : ty_fn) : ty_fn =
    { arg = arg |> sub_ty ~state; result = result |> sub_ty ~state }

  and sub_row :
      'a.
      unite_value:'a Inference.unite ->
      sub_value:(state:interpreter_state -> 'a -> 'a) ->
      state:interpreter_state ->
      'a Row.t ->
      'a Row.t =
   fun ~unite_value ~sub_value ~state row ->
    sub_var
      ~unite_shape:(Row.unite_shape unite_value)
      ~sub_shape:(sub_row_shape ~unite_value ~sub_value)
      ~get_var:(fun (row : 'a Row.t) -> row.var)
      ~new_not_inferred:Row.new_not_inferred ~state row

  and sub_row_shape :
      'a.
      unite_value:'a Inference.unite ->
      sub_value:(state:interpreter_state -> 'a -> 'a) ->
      state:interpreter_state ->
      'a Row.t ->
      'a Row.shape ->
      'a Row.t =
   fun ~unite_value ~sub_value ~state original_row shape ->
    let span = original_row.var |> Inference.Var.spans |> SpanSet.min_elt in
    match shape with
    | R_Error -> Row.inferred ~span R_Error
    | R_Empty -> original_row
    | R_Cons { label; value; rest } ->
        Row.inferred ~span
        <| R_Cons
             {
               label;
               value = sub_value ~state value;
               rest = sub_row ~unite_value ~sub_value ~state rest;
             }

  and sub_var :
      'value 'shape.
      unite_shape:'shape Inference.unite ->
      sub_shape:(state:interpreter_state -> 'value -> 'shape -> 'value) ->
      new_not_inferred:(span:span -> 'value) ->
      get_var:('value -> 'shape Inference.Var.t) ->
      state:interpreter_state ->
      'value ->
      'value =
   fun ~unite_shape ~sub_shape ~new_not_inferred ~get_var ~state
       original_value ->
    let span = Span.fake "<sub_var>" in
    let var = get_var original_value in
    let id = Inference.Var.recurse_id var in
    let cache = RecurseCache.get () in
    let subbed_vars = Effect.perform GetVars in
    if cache |> RecurseCache.visit_count id = 0 then (
      Log.trace (fun log -> log "subbing var %a" Id.print id);
      cache |> RecurseCache.enter id;
      let subbed_temp = new_not_inferred ~span in
      Log.trace (fun log ->
          log "created new var %a" Id.print
            (Inference.Var.recurse_id (get_var subbed_temp)));
      Hashtbl.add subbed_vars id (Obj.repr subbed_temp);
      let subbed_id = Inference.Var.recurse_id (get_var subbed_temp) in
      cache |> RecurseCache.enter subbed_id;
      Hashtbl.add subbed_vars subbed_id (Obj.repr subbed_temp);
      var
      |> Inference.Var.once_inferred (fun shape ->
          with_subbed subbed_vars (fun () ->
              RecurseCache.with_cache cache (fun () ->
                  let subbed = sub_shape ~state original_value shape in
                  Inference.Var.unite ~span unite_shape (get_var subbed_temp)
                    (get_var subbed)
                  |> ignore)));
      cache |> RecurseCache.exit id;
      subbed_temp)
    else
      match Hashtbl.find_opt subbed_vars id with
      | Some sub -> sub |> Obj.obj
      | None -> fail "Failed to find subbed var"
end

let with_cache f =
 fun ~state value ->
  Impl.with_subbed (Hashtbl.create 0) (fun () ->
      let result =
        RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
            f ~state value)
      in
      Impl.subs_count := 0;
      result)

let sub_value ~state value =
  let result = with_cache Impl.sub_value ~state value in
  Log.trace (fun log ->
      log "sub_value from=%a to=%a" Value.print value Value.print result);
  result

let sub_ty ~state ty =
  let result = with_cache Impl.sub_ty ~state ty in
  Log.trace (fun log -> log "sub_ty from=%a to=%a" Ty.print ty Ty.print result);
  result
