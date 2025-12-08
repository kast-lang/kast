open Std
open Kast_util
open Kast_types
open Types

module Impl = struct
  type subbed_vars = Obj.t Inference.Var.Map.t

  type ctx = {
    subs : subbed_vars;
    span : span;
    depth : int;
  }

  let new_ctx ~span = { subs = Inference.Var.Map.create (); span; depth = 0 }
  let go_deeper ctx = { ctx with depth = ctx.depth + 1 }

  type _ Effect.t += GetCtx : ctx Effect.t

  let with_ctx (type a) (ctx : ctx) (f : unit -> a) : a =
    try f () with effect GetCtx, k -> Effect.continue k ctx

  let subs_count = ref 0

  let rec sub_place ~state (place : place) : place =
    match place.state with
    | Occupied value -> Place.init <| sub_value ~state value
    | Uninitialized | MovedOut -> Place.init_state place.state place.ty

  and sub_value ~(state : interpreter_state) (value : value) : value =
    sub_var ~unite_shape:Inference_impl.unite_value_shape
      ~sub_shape:sub_value_shape ~new_not_inferred:Value.new_not_inferred
      ~get_var:(fun (value : value) -> value.var)
      ~state value

  and sub_value_shape ~(state : interpreter_state) (original_value : value)
      (shape : value_shape) : value =
    let span = Span.fake "<sub_value_shape>" in
    Log.trace (fun log ->
        log "subbing value shape = %a" Value.Shape.print shape);
    let shaped shape = Value.inferred ~span shape in
    let result =
      match shape with
      | V_Unit | V_Bool _ | V_Int32 _ | V_Int64 _ | V_Char _ | V_String _
      | V_Ast _ | V_CompilerScope _ | V_Error ->
          original_value
      | V_Ref _ -> original_value (* TODO ??? *)
      | V_Tuple { ty; tuple } ->
          V_Tuple
            {
              ty = sub_ty_tuple ~state ty;
              tuple =
                tuple
                |> Tuple.map
                     (fun
                       ({ place; span; ty_field } : value_tuple_field)
                       :
                       value_tuple_field
                     ->
                       {
                         place = sub_place ~state place;
                         span;
                         ty_field = sub_ty_tuple_field ~state ty_field;
                       });
            }
          |> shaped
      | V_Variant { label; data; ty } ->
          V_Variant
            {
              label;
              data = data |> Option.map (sub_place ~state);
              ty = sub_ty_variant ~state ty;
            }
          |> shaped
      | V_Ty ty -> V_Ty (sub_ty ~state ty) |> shaped
      | V_Fn { ty; fn } -> V_Fn { ty = sub_ty_fn ~state ty; fn } |> shaped
      | V_Generic { id = _; name = _; fn = _ } -> original_value
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
          | Some local -> (
              subs_count := !subs_count + 1;
              match local.place.state with
              | Occupied value -> value
              | Uninitialized | MovedOut ->
                  Error.error span "Tried to sub with %a" Place.print_ref
                    local.place;
                  original_value))
    in
    Log.trace (fun log ->
        log "subbed value shape = %a into %a" Value.Shape.print shape
          Value.print result);
    result

  and sub_optional_name ~state (name : optional_name) : optional_name =
    sub_var
      ~unite_shape:(Inference_impl.unite_option Inference_impl.unite_name_shape)
      ~sub_shape:
        (sub_option ~new_inferred:OptionalName.new_inferred
           ~sub_value:sub_name_shape)
      ~new_not_inferred:OptionalName.new_not_inferred
      ~get_var:(fun (name : optional_name) -> name.var)
      ~state name

  and sub_name_shape ~(state : interpreter_state) ({ parts } : name_shape) :
      name_shape =
    { parts = parts |> List.map (sub_name_part ~state) }

  and sub_name_part ~(state : interpreter_state) (part : name_part) : name_part
      =
    match part with
    | Uri uri -> Uri uri
    | Str s -> Str s
    | Symbol symbol -> Symbol symbol
    | Instantiation value -> Instantiation (sub_value ~state value)

  and sub_option :
      'v 'shape.
      new_inferred:(span:span -> 'shape option -> 'v) ->
      sub_value:(state:interpreter_state -> 'shape -> 'shape) ->
      state:interpreter_state ->
      'v ->
      'shape option ->
      'v =
   fun (type a) ~new_inferred ~sub_value ~state original opt ->
    let ctx = Effect.perform GetCtx in
    match opt with
    | None -> original
    | Some value -> Some (sub_value ~state value) |> new_inferred ~span:ctx.span

  and sub_ty ~state ty =
    sub_var ~unite_shape:Inference_impl.unite_ty_shape ~sub_shape:sub_ty_shape
      ~new_not_inferred:Ty.new_not_inferred
      ~get_var:(fun (ty : ty) -> ty.var)
      ~state ty

  and sub_ty_tuple ~(state : interpreter_state) ({ name; tuple } : ty_tuple) :
      ty_tuple =
    {
      name = sub_optional_name ~state name;
      tuple = tuple |> Tuple.map (fun field -> sub_ty_tuple_field ~state field);
    }

  and sub_ty_variant ~(state : interpreter_state)
      ({ name; variants } : ty_variant) : ty_variant =
    {
      name = sub_optional_name ~state name;
      variants =
        variants
        |> sub_row ~unite_value:Inference_impl.unite_ty_variant_data ~state
             ~sub_value:(fun
                 ~state ({ data } : ty_variant_data) : ty_variant_data ->
               { data = data |> Option.map (sub_ty ~state) });
    }

  and sub_ty_shape ~(state : interpreter_state) (original_ty : ty)
      (shape : ty_shape) : ty =
    let ctx = Effect.perform GetCtx in
    Log.trace (fun log ->
        log "subbing ty shape = %a at %a" Ty.Shape.print shape Span.print
          ctx.span);
    let shaped shape =
      let ty = Ty.inferred ~span:ctx.span shape in
      ctx.subs |> Inference.Var.Map.add ty.var (Obj.repr ty);
      ty
    in
    let result =
      match shape with
      | T_Unit | T_Bool | T_Int32 | T_Int64 | T_String | T_Char | T_Target
      | T_ContextTy | T_CompilerScope | T_Error | T_Ast | T_Ty ->
          original_ty
      | T_Ref inner -> T_Ref (sub_ty ~state inner) |> shaped
      | T_Tuple t -> T_Tuple (sub_ty_tuple ~state t) |> shaped
      | T_Variant t -> T_Variant (sub_ty_variant ~state t) |> shaped
      | T_Fn ty -> T_Fn (sub_ty_fn ~state ty) |> shaped
      | T_Generic { def = _ } -> original_ty
      | T_UnwindToken { result } ->
          T_UnwindToken { result = result |> sub_ty ~state } |> shaped
      | T_Binding binding -> (
          match Scope.find_local_opt binding.name state.scope with
          | None -> original_ty
          | Some local -> (
              subs_count := !subs_count + 1;

              match local.place.state with
              | Occupied value -> (
                  match value |> Value.expect_ty with
                  | Some ty -> ty
                  | None ->
                      Error.error ctx.span
                        "substituted type binding with non-type";
                      T_Error |> shaped)
              | Uninitialized | MovedOut ->
                  Error.error ctx.span "Tried to use sub with %a"
                    Place.print_ref local.place;
                  original_ty))
    in
    Log.trace (fun log ->
        log "subbed ty shape = %a into %a" Ty.Shape.print shape Ty.print result);
    result

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
    let inferred ~span value =
      let result = Row.inferred ~span value in
      let ctx = Effect.perform GetCtx in
      ctx.subs |> Inference.Var.Map.add result.var (Obj.repr result);
      result
    in
    match shape with
    | R_Error -> inferred ~span R_Error
    | R_Empty -> original_row
    | R_Cons { label; value; rest } ->
        inferred ~span
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
    let ctx = Effect.perform GetCtx in
    if ctx.depth > 32 then fail "Went too deep" ~span
    else
      match ctx.subs |> Inference.Var.Map.find_opt var with
      | None ->
          let id = Inference.Var.recurse_id var in
          Log.trace (fun log ->
              log "subbing var %a at %a" Id.print id Span.print span);
          let subbed_temp = new_not_inferred ~span in
          Log.trace (fun log ->
              log "created new var %a at %a" Id.print
                (Inference.Var.recurse_id (get_var subbed_temp))
                Span.print span);
          ctx.subs |> Inference.Var.Map.add var (Obj.repr subbed_temp);
          ctx.subs
          |> Inference.Var.Map.add (get_var subbed_temp) (Obj.repr subbed_temp);
          var
          |> Inference.Var.once_inferred (fun shape ->
              Log.trace (fun log ->
                  log "var %a was inferred, resuming subbing" Id.print id);
              with_ctx (go_deeper ctx) (fun () ->
                  let subbed = sub_shape ~state original_value shape in
                  Inference.Var.unite ~span unite_shape (get_var subbed_temp)
                    (get_var subbed)
                  |> ignore));
          subbed_temp
      | Some sub -> sub |> Obj.obj
end

let with_cache ~span f =
 fun ~state value ->
  Impl.with_ctx (Impl.new_ctx ~span) (fun () ->
      let result = f ~state value in
      Impl.subs_count := 0;
      result)

let sub_value ~span ~state value =
  Log.trace (fun log -> log "sub_value begin at %a" Span.print span);
  let result = with_cache ~span Impl.sub_value ~state value in
  Log.trace (fun log ->
      log "sub_value end at %a from %a to %a" Span.print span Value.print value
        Value.print result);
  result

let sub_ty ~span ~state ty =
  let result = with_cache ~span Impl.sub_ty ~state ty in
  result

type mode =
  | None
  | TyOnly
  | FnOnly
  | Full
