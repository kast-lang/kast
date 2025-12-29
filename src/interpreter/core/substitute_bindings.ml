open Std
open Kast_util
open Kast_types
open Types
open Common

module Impl = struct
  module Interpreter = struct
    let instantiate :
        (?result_ty:ty -> span -> interpreter_state -> value -> value -> value)
        option
        ref =
      ref None

    let get_field :
        (span:span ->
        state:interpreter_state ->
        result_ty:ty ->
        obj_mut:bool ->
        value ->
        Tuple.member ->
        evaled_place_expr)
        option
        ref =
      ref None

    let claim_ref :
        (span:span -> state:interpreter_state -> result_ty:ty -> value -> value)
        option
        ref =
      ref None
  end

  (* target_scope.id -> var -> sub *)
  type subbed_vars = (Id.t option, Obj.t Inference.Var.Map.t) Hashtbl.t

  type ctx = {
    subs : subbed_vars;
    span : span;
    depth : int;
  }

  let new_ctx ~span = { subs = Hashtbl.create 0; span; depth = 0 }
  let go_deeper ctx = { ctx with depth = ctx.depth + 1 }

  type _ Effect.t += GetCtx : ctx Effect.t

  let key ~(state : sub_state) =
    state.result_scope
    |> Option.map (fun (scope : interpreter_scope) -> scope.id)

  let find_sub ~(state : sub_state) var ctx =
    let key = key ~state in
    let* scope_subs = Hashtbl.find_opt ctx.subs key in
    scope_subs |> Inference.Var.Map.find_opt var

  let remember_sub ~(state : sub_state) var sub ctx =
    let scope_subs =
      let key = key ~state in
      match Hashtbl.find_opt ctx.subs key with
      | None ->
          let subs = Inference.Var.Map.create () in
          Hashtbl.add ctx.subs key subs;
          subs
      | Some subs -> subs
    in
    scope_subs |> Inference.Var.Map.add var sub

  let with_ctx (type a) (ctx : ctx) (f : unit -> a) : a =
    try f () with effect GetCtx, k -> Effect.continue k ctx

  let subs_count = ref 0

  let rec sub_place ~state (place : place) : place =
    match place.state with
    | Occupied value -> Place.init ~mut:place.mut <| sub_value ~state value
    | Uninitialized | MovedOut ->
        Place.init_state ~mut:place.mut place.state place.ty

  and sub_value ~(state : sub_state) (value : value) : value =
    sub_var ~unite_shape:Inference_impl.unite_value_shape
      ~sub_shape:sub_value_shape ~new_not_inferred:Value.new_not_inferred
      ~get_var:(fun (value : value) -> value.var)
      ~state value

  and sub_value_shape ~(state : sub_state) (original_value : value)
      (shape : value_shape) : value =
    let ctx = Effect.perform GetCtx in
    let span = ctx.span in
    Log.trace (fun log ->
        log "subbing value shape = %a" Value.Shape.print shape);
    let shaped shape = Value.inferred ~span shape in
    let result =
      match shape with
      | V_Unit | V_Bool _ | V_Int32 _ | V_Int64 _ | V_Float64 _ | V_Char _
      | V_String _ | V_Ast _ | V_CompilerScope _ | V_Error ->
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
      | V_Fn { ty; fn } ->
          let result = V_Fn { ty = sub_ty_fn ~state ty; fn } |> shaped in
          Log.trace (fun log ->
              log "Subbed fn into %a :: %a" Value.print result Ty.print
                result.ty);
          result
      | V_Generic { name; fn; ty } ->
          V_Generic
            {
              name = sub_name_shape ~state name;
              fn;
              ty = sub_ty_generic ~state ty;
            }
          |> shaped
      | V_NativeFn { id; name; ty; impl } ->
          V_NativeFn { id; name; ty = sub_ty_fn ~state ty; impl } |> shaped
      | V_UnwindToken { id; result_ty } ->
          V_UnwindToken { id; result_ty = sub_ty ~state result_ty } |> shaped
      | V_Target _ -> original_value
      | V_ContextTy { id; ty } ->
          V_ContextTy { id; ty = sub_ty ~state ty } |> shaped
      | V_Opaque { ty; value } ->
          V_Opaque { ty = sub_ty_opaque ~state ty; value } |> shaped
      | V_Blocked blocked -> sub_blocked ~original_value ~state blocked
    in
    Log.trace (fun log ->
        log "subbed value shape = %a into %a" Value.Shape.print shape
          Value.print result);
    result

  and sub_blocked ~original_value ~state (blocked : blocked_value) : value =
    let ctx = Effect.perform GetCtx in
    let span = ctx.span in
    match blocked.shape with
    | BV_Instantiate { generic; arg } ->
        let generic =
          sub_blocked
            ~original_value:(Value.inferred ~span (V_Blocked generic))
            ~state generic
        in
        let arg = sub_value ~state arg in
        let instantiate = !Interpreter.instantiate |> Option.get in
        instantiate ~result_ty:blocked.ty span state generic arg
    | BV_Binding binding -> (
        match Scope.find_local_opt binding.name state.scope with
        | None ->
            if not (binding.scope |> VarScope.contains state.result_scope) then
              Error.error span "%a can't escape scope" Binding.print binding;
            original_value
        | Some local -> (
            subs_count := !subs_count + 1;
            match local.place.state with
            | Occupied value ->
                Log.trace (fun log ->
                    log "SUB BINDING %a into %a" Print.print_binding binding
                      Print.print_value value);
                value
            | Uninitialized | MovedOut ->
                Error.error span "Tried to sub with %a" Place.print_ref
                  local.place;
                original_value))
    | BV_FieldRef { obj_ref; member } -> (
        let obj_ref =
          sub_blocked
            ~original_value:(Value.inferred ~span (V_Blocked obj_ref))
            ~state obj_ref
        in
        let get_field = !Interpreter.get_field |> Option.get in
        match
          get_field ~result_ty:blocked.ty ~span ~state ~obj_mut:true obj_ref
            member
        with
        | RefBlocked ref -> V_Blocked ref |> Value.inferred ~span
        | Place (~mut, place) -> V_Ref { mut; place } |> Value.inferred ~span)
    | BV_ClaimRef ref ->
        let ref =
          sub_blocked
            ~original_value:(Value.inferred ~span (V_Blocked ref))
            ~state ref
        in
        let claim_ref = !Interpreter.claim_ref |> Option.get in
        claim_ref ~result_ty:blocked.ty ~span ~state ref

  and sub_name ~state (name : name) : name =
    let ctx = Effect.perform GetCtx in
    sub_var ~unite_shape:Inference_impl.unite_name_shape
      ~sub_shape:(fun ~state (_original : name) shape ->
        sub_name_shape ~state shape |> Name.new_inferred ~span:ctx.span)
      ~new_not_inferred:Name.new_not_inferred
      ~get_var:(fun (name : name) -> name.var)
      ~state name

  and sub_optional_name ~state (name : optional_name) : optional_name =
    sub_var
      ~unite_shape:(Inference_impl.unite_option Inference_impl.unite_name_shape)
      ~sub_shape:
        (sub_option ~new_inferred:OptionalName.new_inferred
           ~sub_value:sub_name_shape)
      ~new_not_inferred:OptionalName.new_not_inferred
      ~get_var:(fun (name : optional_name) -> name.var)
      ~state name

  and sub_name_shape ~(state : sub_state) (shape : name_shape) : name_shape =
    let ctx = Effect.perform GetCtx in
    match shape with
    | Simple part -> Simple (sub_name_part ~state part)
    | Concat (a, b) -> Concat (sub_name_shape ~state a, sub_name_part ~state b)
    | Instantiation { generic; arg } ->
        Log.trace (fun log ->
            log "subbing instantiation %a[%a] at %a" Value.print generic
              Value.print arg Span.print ctx.span);
        let generic = generic |> sub_value ~state in
        let arg = arg |> sub_value ~state in
        Log.trace (fun log ->
            log "subbed instantiation %a[%a] at %a" Value.print generic
              Value.print arg Span.print ctx.span);
        Instantiation { generic; arg }

  and sub_name_part ~(state : sub_state) (part : name_part) : name_part =
    match part with
    | Uri uri -> Uri uri
    | Str s -> Str s
    | Symbol symbol -> Symbol symbol

  and sub_option :
      'v 'shape.
      new_inferred:(span:span -> 'shape option -> 'v) ->
      sub_value:(state:sub_state -> 'shape -> 'shape) ->
      state:sub_state ->
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

  and sub_ty_tuple ~(state : sub_state) ({ name; tuple } : ty_tuple) : ty_tuple
      =
    {
      name = sub_optional_name ~state name;
      tuple = tuple |> Tuple.map (fun field -> sub_ty_tuple_field ~state field);
    }

  and sub_ty_variant ~(state : sub_state)
      ({ name; variants } as ty : ty_variant) : ty_variant =
    let name = sub_optional_name ~state name in
    let variants =
      variants
      |> sub_row ~scope_of_value:VarScope.of_ty_variant_data
           ~unite_value:Inference_impl.unite_ty_variant_data ~state
           ~sub_value:(fun
               ~state ({ data } : ty_variant_data) : ty_variant_data ->
             {
               data =
                 data
                 |> Option.map (fun ty ->
                     let result = sub_ty ~state ty in
                     Log.trace (fun log ->
                         log "subbed variant data ty from %a to %a" Ty.print ty
                           Ty.print result);
                     result);
             })
    in
    let result = { name; variants } in
    Log.trace (fun log ->
        log "subbed ty_variant from %a (row scope = %a) to %a"
          Print.print_ty_variant ty Print.print_var_scope
          (Row.scope ty.variants) Print.print_ty_variant result);
    result

  and sub_ty_opaque ~state ({ name } : ty_opaque) : ty_opaque =
    { name = sub_name ~state name }

  and sub_ty_shape ~(state : sub_state) (original_ty : ty) (shape : ty_shape) :
      ty =
    let ctx = Effect.perform GetCtx in
    Log.trace (fun log ->
        log "subbing ty shape = %a at %a" Ty.Shape.print shape Span.print
          ctx.span);
    let shaped shape =
      let ty = Ty.inferred ~span:ctx.span shape in
      ctx |> remember_sub ~state ty.var (Obj.repr ty);
      ty
    in
    let result =
      match shape with
      | T_Unit | T_Bool | T_Int32 | T_Int64 | T_Float64 | T_String | T_Char
      | T_Target | T_ContextTy | T_CompilerScope | T_Error | T_Ast | T_Ty ->
          original_ty
      | T_Opaque ty -> T_Opaque (sub_ty_opaque ~state ty) |> shaped
      | T_Ref { mut; referenced } ->
          T_Ref { mut; referenced = sub_ty ~state referenced } |> shaped
      | T_Tuple t -> T_Tuple (sub_ty_tuple ~state t) |> shaped
      | T_Variant t -> T_Variant (sub_ty_variant ~state t) |> shaped
      | T_Fn ty -> T_Fn (sub_ty_fn ~state ty) |> shaped
      | T_Generic ty -> T_Generic (sub_ty_generic ~state ty) |> shaped
      | T_UnwindToken { result } ->
          T_UnwindToken { result = result |> sub_ty ~state } |> shaped
      | T_Blocked blocked -> (
          let value =
            sub_blocked
              ~original_value:
                (V_Blocked blocked |> Value.inferred ~span:ctx.span)
              ~state blocked
          in
          match value |> Value.expect_ty with
          | Some ty -> ty
          | None ->
              Error.error ctx.span "substituted type binding with non-type";
              T_Error |> shaped)
    in
    Log.trace (fun log ->
        log "subbed ty shape = %a into %a" Ty.Shape.print shape Ty.print result);
    result

  and sub_ty_generic ~(state : sub_state)
      ({ arg; result } as generic : ty_generic) : ty_generic =
    let ctx = Effect.perform GetCtx in
    let inner_state =
      {
        state with
        scope =
          {
            id = Id.gen ();
            depth = state.scope.depth + 1;
            span = ctx.span;
            parent = Some state.scope;
            locals = Scope.Locals.empty;
            recursive = false;
            closed = false;
            on_update = [];
          };
        result_scope = state.result_scope |> VarScope.enter ~span:ctx.span;
      }
    in
    Log.trace (fun log ->
        log "subbing generic = %a" Print.print_ty_generic generic);
    Log.trace (fun log ->
        log "inner scope = %a" Print.print_var_scope inner_state.result_scope);
    let arg = sub_pattern_and_inject_replacements ~state:inner_state arg in
    let result = sub_ty ~state:inner_state result in
    let generic : ty_generic = { arg; result } in
    Log.trace (fun log ->
        log "subbed generic = %a" Print.print_ty_generic generic);
    generic

  and sub_ty_tuple_field ~(state : sub_state) ({ label; ty } : ty_tuple_field) :
      ty_tuple_field =
    { label; ty = sub_ty ~state ty }

  and sub_ty_fn ~state ({ arg; result } : ty_fn) : ty_fn =
    { arg = arg |> sub_ty ~state; result = result |> sub_ty ~state }

  and sub_pattern_and_inject_replacements :
      state:sub_state -> pattern -> pattern =
   fun ~state pattern ->
    {
      shape = sub_pattern_shape_and_inject_replacements ~state pattern.shape;
      data = sub_ir_data ~state pattern.data;
    }

  and sub_pattern_shape_and_inject_replacements :
      state:sub_state -> pattern_shape -> pattern_shape =
   fun ~state shape ->
    match shape with
    | P_Placeholder -> P_Placeholder
    | P_Ref referenced ->
        P_Ref (sub_pattern_and_inject_replacements ~state referenced)
    | P_Unit -> P_Unit
    | P_Binding { by_ref : bool; binding = old_binding } ->
        let new_binding : binding =
          {
            id = Id.gen ();
            scope = state.result_scope;
            span = old_binding.span;
            name = old_binding.name;
            ty = sub_ty ~state old_binding.ty;
            mut : bool = old_binding.mut;
            label = old_binding.label;
          }
        in
        Log.trace (fun log ->
            log "NEW BINDING %a in scope %a" Print.print_binding new_binding
              Print.print_var_scope new_binding.scope);
        let binding_value =
          V_Blocked { shape = BV_Binding new_binding; ty = new_binding.ty }
          |> Value.inferred ~span:new_binding.span
        in
        Log.trace (fun log ->
            log "NEW BINDING %a in scope %a = %a :: %a" Print.print_binding
              new_binding Print.print_var_scope new_binding.scope
              Print.print_value binding_value Print.print_ty new_binding.ty);
        state.scope
        |> Scope.add_local new_binding.span ~mut:false new_binding.name
             binding_value;
        P_Binding { by_ref; binding = new_binding }
    | P_Tuple { parts } ->
        P_Tuple
          {
            parts =
              parts
              |> List.map
                   (fun
                     (part : pattern tuple_part_of) : pattern tuple_part_of ->
                     match part with
                     | Field { label; label_span; field } ->
                         Field
                           {
                             label;
                             label_span;
                             field =
                               sub_pattern_and_inject_replacements ~state field;
                           }
                     | Unpack packed ->
                         Unpack
                           (sub_pattern_and_inject_replacements ~state packed));
          }
    | P_Variant { label; label_span; value } ->
        P_Variant
          {
            label;
            label_span;
            value =
              value |> Option.map (sub_pattern_and_inject_replacements ~state);
          }
    | P_Error -> P_Error

  and sub_ir_data : state:sub_state -> ir_data -> ir_data =
   fun ~state data ->
    {
      evaled = { patterns = []; ty_exprs = []; exprs = []; ty_ascribed = false };
      compiler_scope = data.compiler_scope;
      span = data.span;
      included_file = None;
      ty = sub_ty ~state data.ty;
    }

  and sub_row :
      'a.
      unite_value:'a Inference.unite ->
      scope_of_value:('a -> var_scope) ->
      sub_value:(state:sub_state -> 'a -> 'a) ->
      state:sub_state ->
      ('a, var_scope) Row.t ->
      ('a, var_scope) Row.t =
   fun ~unite_value ~scope_of_value ~sub_value ~state row ->
    sub_var
      ~unite_shape:
        (Row.unite_shape
           (module Inference_impl.VarScope)
           scope_of_value unite_value)
      ~sub_shape:(sub_row_shape ~scope_of_value ~unite_value ~sub_value)
      ~get_var:(fun (row : ('a, var_scope) Row.t) -> row.var)
      ~new_not_inferred:Row.new_not_inferred ~state row

  and sub_row_shape :
      'a.
      scope_of_value:('a -> var_scope) ->
      unite_value:'a Inference.unite ->
      sub_value:(state:sub_state -> 'a -> 'a) ->
      state:sub_state ->
      ('a, var_scope) Row.t ->
      ('a, var_scope) Row.shape ->
      ('a, var_scope) Row.t =
   fun ~scope_of_value ~unite_value ~sub_value ~state original_row shape ->
    let span = original_row.var |> Inference.Var.spans |> SpanSet.min_elt in
    let inferred ~span value =
      let result =
        Row.inferred (module Inference_impl.VarScope) scope_of_value ~span value
      in
      let ctx = Effect.perform GetCtx in
      ctx |> remember_sub ~state result.var (Obj.repr result);
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
               rest =
                 sub_row ~scope_of_value ~unite_value ~sub_value ~state rest;
             }

  and sub_var :
      'value 'shape.
      unite_shape:'shape Inference.unite ->
      sub_shape:(state:sub_state -> 'value -> 'shape -> 'value) ->
      new_not_inferred:(scope:var_scope -> span:span -> 'value) ->
      get_var:('value -> 'shape var) ->
      state:sub_state ->
      'value ->
      'value =
   fun ~unite_shape ~sub_shape ~new_not_inferred ~get_var ~state
       original_value ->
    let ctx = Effect.perform GetCtx in
    let span = ctx.span in
    let var = get_var original_value in
    let var_scope = var |> Inference.Var.scope in
    if var_scope |> VarScope.contains state.result_scope then (
      Log.trace (fun log ->
          log "not subbing %a into %a" Print.print_var_scope var_scope
            Print.print_var_scope state.result_scope);
      original_value)
    else if ctx.depth > 32 then fail "Went too deep" ~span
    else
      match ctx |> find_sub ~state var with
      | None ->
          let id = Inference.Var.recurse_id var in
          let subbed_temp = new_not_inferred ~scope:state.result_scope ~span in
          let subbed_temp_var = subbed_temp |> get_var in
          ctx |> remember_sub ~state var (Obj.repr subbed_temp);
          ctx |> remember_sub ~state subbed_temp_var (Obj.repr subbed_temp);
          var
          |> Inference.Var.once_inferred (fun shape ->
              Log.trace (fun log ->
                  log "var %a was inferred, resuming subbing" Id.print id);
              with_ctx (go_deeper ctx) (fun () ->
                  let subbed = sub_shape ~state original_value shape in
                  let subbed_var = get_var subbed in
                  Inference.Var.unite unite_shape VarScope.unite ~span
                    subbed_temp_var subbed_var
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

let () = Kast_types.Inference_impl.sub_ty := Some sub_ty

type mode =
  | None
  | TyOnly
  | FnOnly
  | Full
