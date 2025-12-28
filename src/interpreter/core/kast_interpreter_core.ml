open Std
open Kast_util
open Kast_types
open Common
module Ast = Kast_ast
module Error = Error
module Inference = Kast_inference
module Scope = Scope
module Substitute_bindings = Substitute_bindings

type state = Types.interpreter_state
type _ Effect.t += AwaitCompiled : Types.maybe_compiled_fn -> unit Effect.t
type _ Effect.t += AwaitCompiledTyExpr : Expr.ty -> unit Effect.t

exception
  Unwind of {
    token : Types.value_unwind_token;
    value : value;
  }

let is_copy_ty (_ty : ty) : bool = true

let read_place ~span (place : place) : value =
  match place.state with
  | Occupied value -> value
  | Uninitialized ->
      Error.error span "place is not initialized";
      V_Error |> Value.inferred ~span
  | MovedOut ->
      Error.error span "place has been moved out of";
      V_Error |> Value.inferred ~span

let claim ~span (place : place) : value =
  match place.state with
  | Occupied value ->
      if is_copy_ty place.ty then value
      else (
        place.state <- MovedOut;
        value)
  | Uninitialized ->
      Error.error span "place is not initialized";
      V_Error |> Value.inferred ~span
  | MovedOut ->
      Error.error span "place has been moved out of";
      V_Error |> Value.inferred ~span

let claim_ ~span ((~mut:_, place) : mut:bool * place) : value =
  claim ~span place

let claim_ref ~span ~state ~result_ty ref : value =
  match ref |> Value.await_inferred with
  | V_Ref { mut; place } -> claim_ ~span (~mut, place)
  | V_Blocked blocked ->
      V_Blocked { shape = BV_ClaimRef blocked; ty = result_ty }
      |> Value.inferred ~span
  | _ ->
      Error.error span "Expected a ref";
      V_Error |> Value.inferred ~span

(* TODO do in two steps - can_match and perform_match *)
let rec pattern_match :
    span:span -> place -> pattern -> (matched:bool * Scope.locals) =
 fun ~span place pattern ->
  match pattern.shape with
  | P_Placeholder -> (~matched:true, Scope.Locals.empty)
  | P_Unit ->
      (* TODO assert that value is unit *)
      (~matched:true, Scope.Locals.empty)
  | P_Ref inner -> (
      match place |> claim ~span |> Value.expect_ref with
      | None ->
          Error.error span "Expected a ref";
          (~matched:false, Scope.Locals.empty)
      | Some ref -> pattern_match ~span ref.place inner)
  | P_Binding { by_ref; binding } ->
      (* TODO ref mut *)
      let mut = false in
      let ty, value =
        if by_ref then
          ( Ty.inferred ~span
              (T_Ref
                 {
                   mut = IsMutable.new_inferred ~span mut;
                   referenced = place.ty;
                 }),
            Value.inferred ~span (V_Ref { mut; place }) )
        else (place.ty, claim ~span place)
      in
      ( ~matched:true,
        {
          by_symbol =
            SymbolMap.singleton binding.name
              ({
                 place = Place.init ~mut:(Place.Mut.bool binding.mut) value;
                 ty_field = { ty; label = Some binding.label };
               }
                : Types.interpreter_local);
        } )
  | P_Tuple { parts } -> (
      match place |> claim ~span |> Value.await_inferred with
      | V_Tuple { ty = _; tuple } ->
          let matched = ref true in
          let matches = ref Scope.Locals.empty in
          let update_matches =
           fun (~matched:new_matched, (new_matches : Scope.Locals.t)) ->
            if not new_matched then matched := false;
            matches :=
              {
                by_symbol =
                  SymbolMap.union
                    (fun symbol a _b ->
                      Error.error span "%a is matched multiple times in pattern"
                        Symbol.print symbol;
                      Some a)
                    !matches.by_symbol new_matches.by_symbol;
              }
          in
          let next_indexed_field = ref 0 in
          parts
          |> List.iter (fun (part : _ Types.tuple_part_of) ->
              match part with
              | Field { label; label_span = _; field = field_pattern } -> (
                  let member : Tuple.member =
                    match label with
                    | None ->
                        let idx = !next_indexed_field in
                        next_indexed_field := idx + 1;
                        Index idx
                    | Some label -> Name (Label.get_name label)
                  in
                  match Tuple.get_opt member tuple with
                  | Some field ->
                      pattern_match ~span field.place field_pattern
                      |> update_matches
                  | None ->
                      Error.error span "No field %a" Tuple.Member.print member)
              | Unpack packed_pattern -> (
                  match
                    packed_pattern.data.ty |> Ty.await_inferred
                    |> Ty.Shape.expect_tuple
                  with
                  | Some ty_tuple ->
                      let tuple =
                        ty_tuple.tuple
                        |> Tuple.mapi
                             (fun member (field : Types.ty_tuple_field) ->
                               let member : Tuple.member =
                                 match member with
                                 | Index _ ->
                                     let idx = !next_indexed_field in
                                     next_indexed_field := idx + 1;
                                     Index idx
                                 | Name name -> Name name
                               in
                               tuple |> Tuple.get member)
                      in
                      let placed =
                        Place.init ~mut:Inherit
                          (V_Tuple { tuple; ty = ty_tuple }
                          |> Value.inferred ~span)
                      in
                      pattern_match ~span placed packed_pattern
                      |> update_matches
                  | None -> Error.error span "packed pattern type is not tuple?"
                  ));
          (~matched:!matched, !matches)
      | value ->
          Error.error pattern.data.span "Expected tuple, got %a"
            Value.Shape.print value;
          (~matched:false, Scope.Locals.empty))
  | P_Variant { label = patern_label; label_span = _; value = value_pattern }
    -> (
      match place |> claim ~span |> Value.await_inferred with
      | V_Variant { label; data; ty = _ } ->
          if Label.equal label patern_label then
            match (data, value_pattern) with
            | None, None -> (~matched:true, Scope.Locals.empty)
            | None, Some _ ->
                Error.error pattern.data.span "Expected value, got no value";
                (~matched:false, Scope.Locals.empty)
            | Some _, None ->
                Error.error pattern.data.span "Expected no value, got value";
                (~matched:false, Scope.Locals.empty)
            | Some value, Some pattern -> pattern_match ~span value pattern
          else (~matched:false, Scope.Locals.empty)
      | _ ->
          Error.error pattern.data.span "Expected variant";
          (~matched:false, Scope.Locals.empty))
  | P_Error -> (~matched:false, Scope.Locals.empty)

and await_compiled ~span (f : Types.maybe_compiled_fn) :
    Types.compiled_fn option =
  if f.compiled |> Option.is_none then (
    Log.trace (fun log -> log "waiting for fn to compile at %a" Span.print span);
    Effect.perform (AwaitCompiled f);
    Log.trace (fun log -> log "waited for fn to compile at %a" Span.print span));
  f.compiled

and await_compiled_ty_expr ~span (expr : Expr.ty) : Expr.Ty.Shape.t =
  if expr.compiled_shape |> Option.is_none then
    Effect.perform (AwaitCompiledTyExpr expr);
  expr.compiled_shape |> Option.get

and call_untyped_fn ~(sub_mode : Substitute_bindings.mode) (span : span)
    (state : state) (fn : Types.value_untyped_fn) (arg : value) : value =
  with_return (fun { return } ->
      Log.trace (fun log -> log "Started call at %a" Span.print span);
      let def =
        await_compiled ~span fn.def
        |> Option.unwrap_or_else (fun () ->
            Error.error span "Can't call fn, its not compile yet";
            return (V_Error |> Value.inferred ~span))
      in
      Log.trace (fun log -> log "Fn for call at %a is compiled" Span.print span);
      Log.trace (fun log ->
          log "fn arg (%a) = %a"
            (Pattern.print ~options:{ spans = false; types = false })
            def.arg Value.print arg);
      let ~matched:arg_matched, arg_bindings =
        pattern_match ~span (Place.init ~mut:Inherit arg) def.arg
      in
      if not arg_matched then
        Error.error span "Failed to pattern match fn's arg";
      let new_state =
        {
          state with
          scope =
            Scope.with_values ~span:def.body.data.span ~recursive:false
              ~parent:(Some fn.captured) arg_bindings;
          current_fn_natives = fn.calculated_natives;
        }
      in
      let result = eval new_state def.body in
      Log.trace (fun log ->
          log "evaled call (before sub) at %a = %a :: %a" Span.print span
            Value.print result Ty.print result.ty);
      let sub_state : Types.sub_state = sub_here new_state in
      let result =
        match sub_mode with
        | None -> result
        | Full -> Substitute_bindings.sub_value ~span ~state:sub_state result
        | FnOnly -> (
            match result |> Value.expect_fn with
            | Some f ->
                Substitute_bindings.sub_value ~span ~state:sub_state result
            | None -> result)
        | TyOnly ->
            Value.inferred ~span
              (V_Ty
                 (match result |> Value.expect_ty with
                 | Some ty ->
                     Substitute_bindings.sub_ty ~span ~state:sub_state ty
                 | None ->
                     Error.error span "Expected ty, got smth else";
                     Ty.inferred ~span T_Error))
      in
      Log.trace (fun log ->
          log "evaled call (after sub) at %a = %a :: %a" Span.print span
            Value.print result Ty.print result.ty);
      result)

and instantiate ?(result_ty : ty option) (span : span) (state : state)
    (generic : value) (arg : value) : value =
  let result =
    match result_ty with
    | Some result_ty ->
        Value.new_not_inferred_of_ty ~scope:state.result_scope ~span result_ty
    | None -> Value.new_not_inferred ~scope:state.result_scope ~span
  in
  Log.trace (fun log ->
      log "%t with arg=%a, result_ty = %a"
        (Span.print_osc8 span String.print "instantiating")
        Value.print arg Ty.print result.ty);
  fork (fun () ->
      Log.trace (fun log ->
          log "Waiting for instantiation name at %a" Span.print span);
      let name = Value.name result in
      Log.trace (fun log ->
          log "Inferred instantiation value at %a name = %a" Span.print span
            Value.print result);
      let name = name.var |> Inference.Var.await_inferred ~error_shape:None in
      (match name with
      | None -> ()
      | Some name ->
          Log.trace (fun log ->
              log "Inferred instantiation at %a name = %a" Span.print span
                Kast_types.Print.print_name_shape name));
      match name with
      | Some (Instantiation { generic = ty_generic; arg = ty_arg }) ->
          generic |> Inference.Value.expect_inferred_as ~span ty_generic;
          arg |> Inference.Value.expect_inferred_as ~span ty_arg
      | _ -> ());
  fork (fun () ->
      let actual_result =
        match generic |> Value.await_inferred with
        | V_Blocked generic ->
            V_Blocked
              { shape = BV_Instantiate { generic; arg }; ty = result.ty }
            |> Value.inferred ~span
        | V_Generic { id; name; fn; ty = _ } -> (
            let save new_state =
              let generic_instantiations =
                match state.instantiated_generics.map |> Id.Map.find_opt id with
                | None -> Types.ValueMap.empty
                | Some instantiations -> instantiations
              in
              let generic_instantiations =
                generic_instantiations |> Types.ValueMap.add arg new_state
              in
              state.instantiated_generics.map <-
                state.instantiated_generics.map
                |> Id.Map.add id generic_instantiations
            in
            let current_state =
              state.instantiated_generics.map |> Id.Map.find_opt id
              |> Option.and_then (fun instantiations ->
                  instantiations |> Types.ValueMap.find_opt arg)
            in
            match current_state with
            | None ->
                let placeholder =
                  Value.new_not_inferred ~scope:state.result_scope ~span
                in
                save placeholder;
                Log.trace (fun log ->
                    log "Instantiating generic with arg=%a at %a placeholder=%a"
                      Value.print arg Span.print span Value.print placeholder);
                fork (fun () ->
                    try
                      let state =
                        {
                          state with
                          current_name = Instantiation { generic; arg };
                        }
                      in
                      let evaluated_result =
                        call_untyped_fn ~sub_mode:Full span state fn arg
                      in
                      Log.trace (fun log ->
                          log
                            "Instantiated generic with arg=%a, result=%a, uniting with=%a"
                            Value.print arg Value.print evaluated_result
                            Value.print placeholder);
                      placeholder
                      |> Inference.Value.expect_inferred_as ~span
                           evaluated_result;
                      Log.trace (fun log ->
                          log "After uniting result=%a" Value.print placeholder)
                    with effect Inference.Var.AwaitUpdate var, k ->
                      Effect.continue k
                        (Effect.perform <| Inference.Var.AwaitUpdate var));
                placeholder
            | Some result ->
                Log.trace (fun log ->
                    log
                      "Using memoized generic instantiation with arg=%a at %a = %a"
                      Value.print arg Span.print span Value.print result);

                result.var
                |> Inference.Var.once_inferred (fun shape ->
                    Log.trace (fun log ->
                        log
                          "Memoized generic instantiation with arg=%a is inferred=%a"
                          Value.print arg Value.Shape.print shape);
                    match shape with
                    | V_Ty ty ->
                        ty.var
                        |> Inference.Var.once_inferred
                             (fun (ty_shape : Ty.Shape.t) ->
                               Log.trace (fun log ->
                                   log
                                     "Memoized generic instantiation inferred as ty=%a"
                                     Ty.Shape.print ty_shape))
                    | _ -> ());
                result)
        | V_Error -> V_Error |> Value.inferred ~span
        | _ ->
            Error.error span "expected generic";
            V_Error |> Value.inferred ~span
      in
      result |> Inference.Value.expect_inferred_as ~span actual_result);
  result

and call (span : span) (state : state) (f : value) (arg : value) : value =
  match f |> Value.await_inferred with
  | V_Fn { fn; ty = _ } -> call_untyped_fn ~sub_mode:None span state fn arg
  | V_NativeFn f -> f.impl ~caller:span ~state arg
  | V_Error -> V_Error |> Value.inferred ~span
  | _ ->
      Error.error span "expected fn";
      V_Error |> Value.inferred ~span

and assign :
    span:span -> state -> Expr.assignee -> parent_mut:bool -> place -> unit =
 fun ~span state assignee ~parent_mut place ->
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit ->
      (* TODO assert that value is unit 🦄 *)
      ()
  | A_Place assignee_place_expr -> (
      match eval_place state assignee_place_expr with
      | RefBlocked _ -> Error.error span "can't assign to blocked value"
      | Place (~mut:assignee_mut, assignee_place) ->
          let value = claim ~span place in
          assignee_place |> Place.assign ~parent_mut:assignee_mut value)
  | A_Tuple { parts } -> (
      let parent_mut = Place.is_mutable ~parent_mut place in
      match place |> claim ~span |> Value.await_inferred with
      | V_Tuple { ty = _; tuple } ->
          let next_indexed_field = ref 0 in
          parts
          |> List.iter (fun (part : _ Types.tuple_part_of) ->
              match part with
              | Field { label; label_span = _; field = assignee_field } -> (
                  let member : Tuple.member =
                    match label with
                    | None ->
                        let idx = !next_indexed_field in
                        next_indexed_field := idx + 1;
                        Index idx
                    | Some label -> Name (Label.get_name label)
                  in
                  match Tuple.get_opt member tuple with
                  | Some field ->
                      assign ~span state assignee_field ~parent_mut field.place
                  | None ->
                      Error.error span "no field %a" Tuple.Member.print member)
              | Unpack _ -> Error.error span "TODO unpack assignee")
      | _ -> Error.error span "Expected tuple")
  | A_Let pattern ->
      let ~matched, new_bindings = pattern_match ~span place pattern in
      if not matched then
        Error.error assignee.data.span "Failed to pattern match";
      state.scope |> Scope.add_locals new_bindings
  | A_Error -> ()

and error_place : ty -> place =
 fun ty -> Place.init_state ~mut:Inherit Uninitialized ty

and eval_field_expr : state -> Types.field_expr -> (Tuple.member, unit) result =
 fun state field ->
  match field with
  | Index i -> Ok (Index i)
  | Name name -> Ok (Name (Label.get_name name))
  | Expr e -> (
      match eval state e |> Value.await_inferred with
      | V_Int32 i -> Ok (Index (Int32.to_int i))
      | V_String s -> Ok (Name s)
      | _ ->
          Error.error e.data.span "field label incorrect type";
          Error ())

and get_field ~span ~state ~(result_ty : ty) ~obj_mut obj member :
    evaled_place_expr =
  with_return (fun { return } ->
      let place : place =
        match obj |> Value.await_inferred with
        | V_Tuple { ty = _; tuple } -> (
            match Tuple.get_opt member tuple with
            | Some field -> field.place
            | None ->
                Error.error span "field %a doesnt exist" Tuple.Member.print
                  member;
                error_place result_ty)
        | V_Target { name } -> (
            match member with
            | Name "name" ->
                Place.init ~mut:Immutable (V_String name |> Value.inferred ~span)
            | _ ->
                Error.error span "field %a doesnt exist in target"
                  Tuple.Member.print member;
                error_place result_ty)
        | V_Blocked blocked ->
            return
            <| RefBlocked
                 {
                   shape = BV_FieldRef { obj_ref = blocked; member };
                   ty = result_ty;
                 }
        | V_Error -> error_place result_ty
        | obj_shape -> (
            match cast_as_module_opt ~span state obj with
            | Some obj_mod ->
                return
                <| get_field ~state ~span ~result_ty ~obj_mut obj_mod member
            | None ->
                Error.error span "%a doesnt have fields" Value.Shape.print
                  obj_shape;
                error_place result_ty)
      in
      Place (~mut:(Place.is_mutable ~parent_mut:obj_mut place), place))

and sub_here : state -> Types.sub_state = fun state -> state

and eval_place : state -> Types.place_expr -> evaled_place_expr =
 fun state expr ->
  try
    let span = expr.data.span in
    Log.trace (fun log -> log "evaluating place at %a" Span.print span);
    let result =
      with_return (fun { return } ->
          match expr.shape with
          | PE_Error -> Place (~mut:true, error_place expr.data.ty)
          | PE_Binding binding ->
              let result =
                Scope.find_opt binding.name state.scope
                |> Option.unwrap_or_else (fun () : place ->
                    Log.trace (fun log ->
                        log "all in scope: %a" Scope.print_all state.scope);
                    Error.error expr.data.span "%a not found" Symbol.print
                      binding.name;
                    error_place expr.data.ty)
              in
              Log.trace (fun log ->
                  log "evaled binding %a = %a" Binding.print binding Id.print
                    result.id);
              Place (~mut:true, result)
          | PE_Temp expr ->
              let value = eval state expr in
              Place (~mut:true, Place.init ~mut:Mutable value)
          | PE_Deref ref_expr -> (
              match eval state ref_expr |> Value.expect_ref with
              | None ->
                  Error.error ref_expr.data.span "Expected a reference";
                  Place (~mut:true, error_place expr.data.ty)
              | Some { mut; place } ->
                  Place (~mut:(Place.is_mutable ~parent_mut:mut place), place))
          | PE_Field { obj; field; field_span = _ } -> (
              let member : Tuple.member =
                match eval_field_expr state field with
                | Ok member -> member
                | Error () ->
                    return <| Place (~mut:true, error_place expr.data.ty)
              in
              match eval_place state obj with
              | RefBlocked obj_ref ->
                  RefBlocked
                    {
                      shape = BV_FieldRef { obj_ref; member };
                      ty =
                        T_Ref
                          {
                            mut =
                              {
                                var =
                                  Inference.Var.new_not_inferred
                                    ~scope:state.result_scope ~span;
                              };
                            referenced = expr.data.ty;
                          }
                        |> Ty.inferred ~span;
                    }
              | Place (~mut:obj_mut, obj) ->
                  let result_ty =
                    Substitute_bindings.sub_ty ~span ~state:(sub_here state)
                      expr.data.ty
                  in
                  get_field ~state ~result_ty ~span ~obj_mut
                    (obj |> read_place ~span)
                    member))
    in
    Log.trace (fun log -> log "evaled place at %a" Span.print span);
    result
  with
  | Unwind _ as exc -> raise exc
  | exc ->
      Log.error (fun log ->
          log "While evaluating place expr at %a" Span.print expr.data.span);
      raise exc

and eval_expr_ref : state -> expr -> Types.expr_ref -> value =
 fun state expr { mut; place } ->
  let span = expr.data.span in
  match eval_place state place with
  | RefBlocked _ ->
      Error.error span "Can't get ref to blocked value";
      V_Error |> Value.inferred ~span
  | Place (~mut:place_mut, place) ->
      if mut && not place_mut then
        Error.error span "Constructing mutable ref to non-mut place";
      V_Ref { mut; place } |> Value.inferred ~span

and eval_expr_claim : state -> expr -> Types.place_expr -> value =
 fun state expr place ->
  let span = expr.data.span in
  match eval_place state place with
  | RefBlocked blocked ->
      let ty =
        Substitute_bindings.sub_ty ~span ~state:(sub_here state) expr.data.ty
      in
      V_Blocked { shape = BV_ClaimRef blocked; ty } |> Value.inferred ~span
  | Place (~mut:_, place) -> place |> claim ~span

and eval_expr_fn : state -> expr -> Types.expr_fn -> value =
 fun state expr { def; ty } ->
  let span = expr.data.span in
  V_Fn
    {
      ty;
      fn =
        {
          id = Id.gen ();
          def;
          captured = state.scope;
          calculated_natives = Hashtbl.create 0;
        };
    }
  |> Value.inferred ~span

and pattern_bindings : pattern -> binding list =
 fun pattern ->
  match pattern.shape with
  | P_Binding { by_ref = _; binding } -> [ binding ]
  | P_Placeholder | P_Unit | P_Error -> []
  | P_Ref referenced -> pattern_bindings referenced
  | P_Tuple { parts } ->
      parts
      |> List.map (fun (part : pattern Types.tuple_part_of) ->
          match part with
          | Field { label = _; label_span = _; field } -> pattern_bindings field
          | Unpack packed -> pattern_bindings packed)
      |> List.flatten
  | P_Variant { label = _; label_span = _; value } -> (
      match value with
      | Some value -> pattern_bindings value
      | None -> [])

and generic_ty : span:span -> pattern -> ty -> Types.ty_generic =
 fun ~span arg result -> { arg; result }

and eval_expr_generic : state -> expr -> Types.expr_generic -> value =
 fun state expr { def; ty } ->
  let span = expr.data.span in
  V_Generic
    {
      id = Id.gen ();
      name = current_name state;
      fn =
        {
          id = Id.gen ();
          def;
          captured = state.scope;
          calculated_natives = Hashtbl.create 0;
        };
      ty;
    }
  |> Value.inferred ~span

and eval_expr_tuple : state -> expr -> Types.expr_tuple -> value =
 fun state expr { parts } ->
  let span = expr.data.span in
  (*  TODO dont panic - get rid of Option.get *)
  let ty =
    expr.data.ty |> Ty.await_inferred |> Ty.Shape.expect_tuple |> Option.get
  in
  let result = ref Tuple.empty in
  parts
  |> List.iter (fun (part : _ Types.tuple_part_of) ->
      match part with
      | Field { label; label_span; field = field_expr } ->
          let name = label |> Option.map Label.get_name in
          let member : Tuple.member =
            match name with
            | None -> Index (!result.unnamed |> Array.length)
            | Some name -> Name name
          in
          let field_value = eval state field_expr in
          let field : Types.value_tuple_field =
            {
              place = Place.init ~mut:Inherit field_value;
              span = label_span;
              ty_field =
                (match ty.tuple |> Tuple.get_opt member with
                | Some ty_field -> ty_field
                | None -> { label; ty = Value.ty_of field_value });
            }
          in
          result := !result |> Tuple.add name field
      | Unpack packed -> (
          let packed = eval state packed in
          match packed |> Value.expect_tuple with
          | Some { tuple; ty = _ } ->
              tuple
              |> Tuple.iter (fun member (field : Types.value_tuple_field) ->
                  let name =
                    match member with
                    | Index _ -> None
                    | Name name -> Some name
                  in
                  let field_value = field.place |> claim ~span in
                  let field : Types.value_tuple_field =
                    {
                      place = Place.init ~mut:Inherit field_value;
                      span = field.span;
                      ty_field = field.ty_field;
                    }
                  in
                  result := !result |> Tuple.add name field)
          | None -> Error.error span "packed was not tuple"));
  V_Tuple
    {
      ty;
      tuple = !result;
      (*        { place = Place.init value; span = label_span; ty_field }); *)
    }
  |> Value.inferred ~span

and eval_expr_variant : state -> expr -> Types.expr_variant -> value =
 fun state expr { label; label_span = _; value } ->
  let span = expr.data.span in
  (*  TODO dont panic - get rid of Option.get *)
  let ty =
    expr.data.ty.var |> Kast_inference_base.Var.inferred_opt |> Option.get
    |> Ty.Shape.expect_variant |> Option.get
  in
  let value = value |> Option.map (eval state) in
  V_Variant { label; data = value |> Option.map (Place.init ~mut:Inherit); ty }
  |> Value.inferred ~span

and eval_expr_then : state -> expr -> Types.expr_then -> value =
 fun state expr { list } ->
  let span = expr.data.span in

  let result = ref (V_Unit |> Value.inferred ~span) in
  list |> List.iter (fun e -> result := eval state e);
  !result

and eval_expr_stmt : state -> expr -> Types.expr_stmt -> value =
 fun state expr { expr } ->
  let span = expr.data.span in

  ignore <| eval state expr;
  V_Unit |> Value.inferred ~span

and eval_expr_scope : state -> expr -> Types.expr_scope -> value =
 fun state expr { expr } ->
  let span = expr.data.span in
  let state = state |> enter_scope ~span:expr.data.span ~recursive:false in
  eval state expr

and eval_expr_assign : state -> expr -> Types.expr_assign -> value =
 fun state expr { assignee; value = value_expr } ->
  let span = expr.data.span in
  match eval_place state value_expr with
  | RefBlocked _ ->
      Error.error span "can't assign blocked values";
      V_Unit |> Value.inferred ~span
  | Place (~mut, place) ->
      assign ~span:value_expr.data.span state assignee ~parent_mut:mut place;
      V_Unit |> Value.inferred ~span

and eval_expr_apply : state -> expr -> Types.expr_apply -> value =
 fun state expr { f; arg } ->
  let span = expr.data.span in
  let f = eval state f in
  let arg = eval state arg in
  call expr.data.span state f arg

and eval_expr_instantiategeneric :
    state -> expr -> Types.expr_instantiate_generic -> value =
 fun state expr { generic; arg } ->
  let span = expr.data.span in
  let generic = eval state generic in
  let arg = eval state arg in
  let result_ty =
    Substitute_bindings.sub_ty ~span ~state:(sub_here state) expr.data.ty
  in
  instantiate ~result_ty expr.data.span state generic arg

and eval_expr_native : state -> expr -> Types.expr_native -> value =
 fun state expr { id; expr = native_expr } ->
  match Hashtbl.find_opt state.current_fn_natives id with
  | Some value -> value
  | None ->
      let value =
        let span = expr.data.span in
        match StringMap.find_opt native_expr state.natives.by_name with
        | Some f ->
            let ty =
              Substitute_bindings.sub_ty ~span ~state:(sub_here state)
                expr.data.ty
            in
            f ty
        | None ->
            Error.error expr.data.span "no native %S" native_expr;
            V_Error |> Value.inferred ~span
      in
      Hashtbl.add state.current_fn_natives id value;
      value

and eval_expr_module : state -> expr -> Types.expr_module -> value =
 fun state expr { def } ->
  let span = expr.data.span in
  let module_scope =
    Scope.init ~span:def.data.span ~recursive:true ~parent:(Some state.scope)
  in
  let new_state = { state with scope = module_scope } in
  ignore @@ eval new_state def;
  Scope.close module_scope;
  let fields =
    module_scope.locals.by_symbol |> SymbolMap.to_list
    |> List.map (fun ((symbol : symbol), (local : Types.interpreter_local)) ->
        ( Some symbol.name,
          ({
             place = local.place;
             span = local.ty_field.label |> Option.get |> Label.get_span;
             ty_field = local.ty_field;
           }
            : Types.value_tuple_field) ))
  in
  (*  TODO dont panic - get rid of Option.get *)
  let ty =
    expr.data.ty.var |> Kast_inference_base.Var.inferred_opt |> Option.get
    |> Ty.Shape.expect_tuple |> Option.get
  in
  V_Tuple { tuple = fields |> Tuple.of_list; ty } |> Value.inferred ~span

and eval_expr_usedotstar : state -> expr -> Types.expr_use_dot_star -> value =
 fun state expr { used; bindings } ->
  let span = expr.data.span in
  let used = eval state used in
  match used |> Value.await_inferred with
  | V_Tuple { ty = _; tuple } ->
      bindings
      |> List.iter (fun (binding : binding) ->
          let field = tuple |> Tuple.get_named binding.name.name in
          state.scope
          |> Scope.add_local_existing_place field.span binding.name field.place);
      V_Unit |> Value.inferred ~span
  | _ ->
      Error.error expr.data.span "can't use .* %a" Value.print used;
      V_Error |> Value.inferred ~span

and eval_expr_if : state -> expr -> Types.expr_if -> value =
 fun state expr { cond = cond_expr; then_case; else_case } ->
  let span = expr.data.span in
  let cond = eval state cond_expr in
  match cond |> Value.await_inferred with
  | V_Bool true -> eval state then_case
  | V_Bool false -> eval state else_case
  | _ ->
      Error.error cond_expr.data.span "if cond must be bool, got %a" Value.print
        cond;
      V_Error |> Value.inferred ~span

and eval_expr_and : state -> expr -> Types.expr_and -> value =
 fun state expr { lhs; rhs } ->
  let span = expr.data.span in
  match eval state lhs |> Value.expect_bool with
  | None ->
      Error.error lhs.data.span "lhs of and evaled not to a bool";
      V_Error |> Value.inferred ~span
  | Some true -> (
      match eval state rhs |> Value.expect_bool with
      | None ->
          Error.error lhs.data.span "rhs of and evaled not to a bool";
          V_Error |> Value.inferred ~span
      | Some rhs -> V_Bool rhs |> Value.inferred ~span)
  | Some false -> V_Bool false |> Value.inferred ~span

and eval_expr_or : state -> expr -> Types.expr_or -> value =
 fun state expr { lhs; rhs } ->
  let span = expr.data.span in
  match eval state lhs |> Value.expect_bool with
  | None ->
      Error.error lhs.data.span "lhs of or evaled not to a bool";
      V_Error |> Value.inferred ~span
  | Some false -> (
      match eval state rhs |> Value.expect_bool with
      | None ->
          Error.error lhs.data.span "rhs of or evaled not to a bool";
          V_Error |> Value.inferred ~span
      | Some rhs -> V_Bool rhs |> Value.inferred ~span)
  | Some true -> V_Bool true |> Value.inferred ~span

and eval_expr_match : state -> expr -> Types.expr_match -> value =
 fun state expr { value = place_expr; branches } ->
  let span = expr.data.span in
  match eval_place state place_expr with
  | RefBlocked _ ->
      Error.error span "Can't match blocked values";
      V_Error |> Value.inferred ~span
  | Place (~mut, place) -> (
      let result =
        branches
        |> List.find_map (fun (branch : Types.expr_match_branch) ->
            let ~matched, matches =
              pattern_match ~span:place_expr.data.span place branch.pattern
            in
            if matched then
              Some
                (let inner_state =
                   enter_scope ~span:branch.body.data.span ~recursive:false
                     state
                 in
                 inner_state.scope |> Scope.add_locals matches;
                 eval inner_state branch.body)
            else None)
      in
      match result with
      | Some result -> result
      | None ->
          Error.error expr.data.span "pattern match non exhaustive";
          V_Error |> Value.inferred ~span)

and eval_expr_loop : state -> expr -> Types.expr_loop -> value =
 fun state expr { body } ->
  let span = expr.data.span in
  let state = state |> enter_scope ~span:body.data.span ~recursive:false in
  while true do
    ignore @@ eval state body
  done

and eval_expr_unwindable : state -> expr -> Types.expr_unwindable -> value =
 fun state expr { token = token_pattern; body } ->
  let span = expr.data.span in
  let id = Id.gen () in
  let token : Types.value_unwind_token =
    {
      id;
      result_ty =
        Substitute_bindings.sub_ty ~span ~state:(sub_here state) body.data.ty;
    }
  in
  let token : value =
    V_UnwindToken token |> Value.inferred ~span:token_pattern.data.span
  in
  let inner_state = enter_scope ~span:body.data.span ~recursive:false state in
  inner_state.scope
  |> Scope.add_locals
       ( pattern_match ~span:token_pattern.data.span
           (Place.init ~mut:Immutable token)
           token_pattern
       |> fun (~matched, locals) ->
         if not matched then
           Error.error token_pattern.data.span
             "Failed to pattern match unwind token";
         locals );
  try eval inner_state body
  with Unwind { token; value } when token.id = id -> value

and eval_expr_unwind : state -> expr -> Types.expr_unwind -> value =
 fun state expr { token = token_expr; value } ->
  let span = expr.data.span in
  with_return (fun { return } ->
      let token =
        eval state token_expr |> Value.expect_unwind_token
        |> Option.unwrap_or_else (fun () ->
            Error.error token_expr.data.span "Unwind token was incorrect type";
            return (V_Error |> Value.inferred ~span : value))
      in
      let value = eval state value in
      raise <| Unwind { token; value })

and eval_expr_injectcontext :
    state -> expr -> Types.expr_inject_context -> value =
 fun state expr { context_ty; value } ->
  let span = expr.data.span in
  let value = eval state value in
  state.contexts <- state.contexts |> Id.Map.add context_ty.id value;
  V_Unit |> Value.inferred ~span

and eval_expr_currentcontext :
    state -> expr -> Types.expr_current_context -> value =
 fun state expr { context_ty } ->
  let span = expr.data.span in
  match state.contexts |> Id.Map.find_opt context_ty.id with
  | Some value -> value
  | None ->
      Error.error expr.data.span "Context unavailable";
      V_Error |> Value.inferred ~span

and eval_expr_implcast : state -> expr -> Types.expr_impl_cast -> value =
 fun state expr { value; target; impl } ->
  let span = expr.data.span in
  let value = eval state value in
  (* let target = eval state target in *)
  let impl = eval state impl in
  let current_target_impls =
    state.cast_impls.map
    |> Types.ValueMap.find_opt target
    |> Option.unwrap_or_else (fun () -> Types.ValueMap.empty)
  in
  let updated_target_impls =
    current_target_impls |> Types.ValueMap.add value impl
  in
  state.cast_impls.map <-
    state.cast_impls.map |> Types.ValueMap.add target updated_target_impls;
  Log.trace (fun log ->
      log "Added cast impl: %a as %a" Value.print value Value.print target);
  V_Unit |> Value.inferred ~span

and eval_expr_cast : state -> expr -> Types.expr_cast -> value =
 fun state expr { value; target } ->
  let span = expr.data.span in
  let value = eval state value in
  let _ : Value.shape = value |> await_fully_inferred in
  let impl =
    state.cast_impls.map
    |> Types.ValueMap.find_opt target
    |> Option.and_then (fun target_impls ->
        target_impls |> Types.ValueMap.find_opt value)
  in
  match impl with
  | Some impl -> impl
  | None ->
      Error.error span "no impl of %a as %a" Value.print value Value.print
        target;
      state.cast_impls.map
      |> Types.ValueMap.iter (fun existing_target impls ->
          impls
          |> Types.ValueMap.iter (fun existing_value _impl ->
              if
                Types.ValueImpl.compare target existing_target = 0
                && Types.ValueImpl.compare value existing_value = 0
              then Log.error (fun log -> log "Not found but actually there????");
              Log.trace (fun log ->
                  log "Exists impl: %a as %a" Value.print existing_value
                    Value.print existing_target)));
      V_Error |> Value.inferred ~span

and await_fully_inferred value : Value.Shape.t =
  (* TODO actual impl *)
  (match value.var |> Inference.Var.inferred_or_default with
  | Some (V_Ty ty) -> ty.var |> Inference.Var.setup_default_if_needed
  | _ -> ());
  Value.await_inferred value

and impl_cast_as_module :
    span:span -> state -> value:value -> impl:value -> unit =
 fun ~span state ~value ~impl ->
  let _ : Value.shape = value |> await_fully_inferred in
  state.cast_impls.as_module <-
    state.cast_impls.as_module
    |> Types.ValueMap.update value (fun current_impl ->
        match current_impl with
        | None -> Some impl
        | Some current_impl ->
            Error.error span "Duplicate impl as module";
            Some current_impl)

and cast_as_module_opt : span:span -> state -> value -> value option =
 fun ~span:_ state value ->
  let _ : Value.shape = value |> await_fully_inferred in
  state.cast_impls.as_module |> Types.ValueMap.find_opt value

and cast_as_module : span:span -> state -> value -> value =
 fun ~span state value ->
  match cast_as_module_opt ~span state value with
  | Some impl -> impl
  | None ->
      Error.error span "no impl of %a as module" Value.print value;
      V_Error |> Value.inferred ~span

and eval_expr_targetdependent :
    state -> expr -> Types.expr_target_dependent -> value =
 fun state expr ({ branches; interpreter_branch } as target_dep_expr) ->
  let span = expr.data.span in
  with_return (fun { return } ->
      let chosen_branch =
        match interpreter_branch with
        | Some computed -> computed
        | None ->
            let computed =
              find_target_dependent_branch state branches
                { name = "interpreter" }
              |> Option.unwrap_or_else (fun () ->
                  Error.error expr.data.span
                    "No target dependent branch matched";
                  return (V_Error |> Value.inferred ~span : value))
            in
            target_dep_expr.interpreter_branch <- Some computed;
            computed
      in
      eval state chosen_branch.body)

and eval_expr_ty : state -> expr -> Types.ty_expr -> value =
 fun state expr e ->
  let span = expr.data.span in
  V_Ty (eval_ty state e) |> Value.inferred ~span

and eval_expr_quote_ast : state -> expr -> Types.expr_quote_ast -> value =
 fun state expr e ->
  let span = expr.data.span in
  V_Ast (quote_ast ~span state e) |> Value.inferred ~span

and eval : state -> expr -> value =
 fun state expr ->
  let span = expr.data.span in
  Kast_profiling.record
    (fun () -> make_string "eval %a" Span.print span)
    (fun () ->
      try
        Log.trace (fun log -> log "evaluating at %a" Span.print span);
        let result =
          match expr.shape with
          | E_Ref place -> eval_expr_ref state expr place
          | E_Claim place -> eval_expr_claim state expr place
          | E_Constant value ->
              Log.info (fun log ->
                  log "const: before sub = %a" Value.print value);
              let result =
                Substitute_bindings.sub_value ~span ~state:(sub_here state)
                  value
              in
              Log.info (fun log ->
                  log "const: after sub = %a" Value.print result);
              result
          | E_Fn f -> eval_expr_fn state expr f
          | E_Generic f -> eval_expr_generic state expr f
          | E_Tuple tuple -> eval_expr_tuple state expr tuple
          | E_Variant e -> eval_expr_variant state expr e
          | E_Then e -> eval_expr_then state expr e
          | E_Stmt e -> eval_expr_stmt state expr e
          | E_Scope e -> eval_expr_scope state expr e
          | E_Assign e -> eval_expr_assign state expr e
          | E_Apply e -> eval_expr_apply state expr e
          | E_InstantiateGeneric e -> eval_expr_instantiategeneric state expr e
          | E_Ty e -> eval_expr_ty state expr e
          | E_Newtype ty_expr ->
              let result_ty =
                Ty.new_not_inferred ~scope:state.result_scope ~span
              in
              fork (fun () ->
                  let inner_ty = eval_ty state ty_expr in
                  let name =
                    OptionalName.new_inferred ~span (Some (current_name state))
                  in
                  let newtype : Ty.Shape.t =
                    match inner_ty |> Ty.await_inferred with
                    | T_Tuple { name = _; tuple } -> T_Tuple { name; tuple }
                    | T_Variant { name = _; variants } ->
                        T_Variant { name; variants }
                    | _ ->
                        Error.error span
                          "Can only newtype variant or tuple types";
                        T_Error
                  in
                  let newtype = Ty.inferred ~span newtype in
                  result_ty |> Inference.Ty.expect_inferred_as ~span newtype);
              V_Ty result_ty |> Value.inferred ~span
          | E_Native e -> eval_expr_native state expr e
          | E_Module e -> eval_expr_module state expr e
          | E_UseDotStar e -> eval_expr_usedotstar state expr e
          | E_If e -> eval_expr_if state expr e
          | E_And e -> eval_expr_and state expr e
          | E_Or e -> eval_expr_or state expr e
          | E_Match e -> eval_expr_match state expr e
          | E_QuoteAst e -> eval_expr_quote_ast state expr e
          | E_Loop e -> eval_expr_loop state expr e
          | E_Error -> V_Error |> Value.inferred ~span
          | E_Unwindable e -> eval_expr_unwindable state expr e
          | E_Unwind e -> eval_expr_unwind state expr e
          | E_InjectContext e -> eval_expr_injectcontext state expr e
          | E_CurrentContext e -> eval_expr_currentcontext state expr e
          | E_ImplCast e -> eval_expr_implcast state expr e
          | E_Cast e -> eval_expr_cast state expr e
          | E_TargetDependent e -> eval_expr_targetdependent state expr e
        in
        (* let result = Substitute_bindings.sub_value ~state result in *)
        Log.trace (fun log ->
            log "evaled at %a = %a" Span.print span Value.print result);
        Log.trace (fun log ->
            log "scope = %a, result scope = %a" Print.print_var_scope
              state.result_scope Print.print_var_scope
              (VarScope.of_value result));
        result
      with
      | Unwind _ as exc -> raise exc
      | exc ->
          Log.error (fun log ->
              log "While evaluating %a expr at %a" Expr.print_short expr
                Span.print expr.data.span);
          raise exc)

and find_target_dependent_branch :
    state ->
    Types.expr_target_dependent_branch list ->
    Types.value_target ->
    Types.expr_target_dependent_branch option =
  let span = Span.of_ocaml __POS__ in
  fun state branches target ->
    branches
    |> List.find_map (fun (branch : Types.expr_target_dependent_branch) ->
        let scope_with_target =
          Scope.init ~span ~recursive:false ~parent:(Some state.scope)
        in
        scope_with_target
        |> Scope.add_local ~mut:false span Types.target_symbol
             (V_Target target |> Value.inferred ~span);
        let state_with_target = { state with scope = scope_with_target } in
        match eval state_with_target branch.cond |> Value.expect_bool with
        | None ->
            Error.error branch.cond.data.span "Cond didn't evaluate to a bool";
            None
        | Some cond -> if cond then Some branch else None)

and quote_ast : span:span -> state -> Expr.Shape.quote_ast -> Ast.t =
 fun ~span state expr ->
  let rec quote_group (group : Expr.Shape.quote_ast_group) : Ast.group =
    {
      rule = group.rule;
      parts = [];
      children =
        group.children
        |> Tuple.map (fun (child : Expr.Shape.quote_ast_child) : Ast.child ->
            match child with
            | Group child_group -> Group (quote_group child_group)
            | Ast child ->
                Ast
                  (let child = eval state child in
                   match child |> Value.await_inferred with
                   | V_Ast ast -> ast
                   | _ -> fail "child must be ast"));
    }
  in
  { shape = Complex { rule = expr.rule; root = quote_group expr.root }; span }

and current_name : state -> Types.name_shape =
 fun state ->
  Log.trace (fun log ->
      log "Current name = %a" Kast_types.Print.print_name_shape
        state.current_name);
  state.current_name

and eval_ty : state -> Expr.ty -> ty =
 fun state expr ->
  let span = expr.data.span in
  let result = Ty.new_not_inferred ~scope:state.result_scope ~span in
  fork (fun () ->
      try
        Log.trace (fun log -> log "started eval ty expr at %a" Span.print span);
        let evaled_result =
          match await_compiled_ty_expr ~span:expr.data.span expr with
          | TE_Unit -> Ty.inferred ~span T_Unit
          | TE_Ref { mut; referenced } ->
              let referenced = eval_ty state referenced in
              Ty.inferred ~span <| T_Ref { mut; referenced }
          | TE_Fn { arg; result } ->
              let arg = eval_ty state arg in
              let result = eval_ty state result in
              Ty.inferred ~span <| T_Fn { arg; result }
          | TE_Expr expr ->
              let value = eval state expr in
              Log.trace (fun log ->
                  log "evaled ty expr (expr) at %a = %a" Span.print span
                    Value.print value);
              value |> Value.expect_ty
              |> Option.unwrap_or_else (fun () ->
                  Error.error expr.data.span "Expected a type, got %a" Ty.print
                    (Value.ty_of value);
                  Ty.inferred ~span T_Error)
          | TE_Tuple { parts } ->
              let tuple = ref Tuple.empty in
              parts
              |> List.iter (fun (part : _ Types.tuple_part_of) ->
                  match part with
                  | Field { label; label_span = _; field = field_expr } ->
                      let name = label |> Option.map Label.get_name in
                      let ty = field_expr |> eval_ty state in
                      let ty_field : Types.ty_tuple_field = { ty; label } in
                      tuple := !tuple |> Tuple.add name ty_field
                  | Unpack packed -> (
                      let packed = eval_ty state packed in
                      match packed |> Ty.await_inferred with
                      | T_Tuple { name = _; tuple = packed } ->
                          packed
                          |> Tuple.iter (fun member field ->
                              let name =
                                match member with
                                | Index _ -> None
                                | Name name -> Some name
                              in
                              tuple := !tuple |> Tuple.add name field)
                      | _ -> Error.error span "can only unpack tuples"));
              Ty.inferred ~span
              <| T_Tuple
                   {
                     name = OptionalName.new_inferred ~span None;
                     tuple = !tuple;
                   }
          | TE_Union { elements } ->
              let variants =
                elements
                |> List.map (fun ty_expr ->
                    let ty = eval_ty state ty_expr in
                    match ty |> Ty.await_inferred with
                    | T_Variant { name = _; variants } ->
                        Row.await_inferred_to_list variants
                    | _ ->
                        Error.error ty_expr.data.span
                          "Can only use variants in union";
                        [])
                |> List.flatten
              in
              Ty.inferred ~span
              <| T_Variant
                   {
                     name = OptionalName.new_inferred ~span None;
                     variants =
                       Row.of_list
                         (module VarScope)
                         VarScope.of_ty_variant_data ~span variants;
                   }
          | TE_Variant { variants } ->
              Ty.inferred ~span
              <| T_Variant
                   {
                     name = OptionalName.new_inferred ~span None;
                     variants =
                       Row.of_list
                         (module VarScope)
                         VarScope.of_ty_variant_data ~span
                         (variants
                         |> List.map
                              (fun
                                ({ label_span = _; label; value } :
                                  Types.ty_expr_variant_variant)
                              ->
                                ( label,
                                  ({
                                     data = value |> Option.map (eval_ty state);
                                   }
                                    : Types.ty_variant_data) )));
                   }
          | TE_Error -> Ty.inferred ~span T_Error
        in
        result
        |> Inference.Ty.expect_inferred_as ~span:expr.data.span evaled_result;
        Log.trace (fun log ->
            log "finished eval ty expr at %a = %a" Span.print span Ty.print
              evaled_result);
        Log.trace (fun log ->
            log "scope = %a, result scope = %a" Print.print_var_scope
              state.result_scope Print.print_var_scope
              (VarScope.of_ty evaled_result))
      with
      | effect Scope.AwaitUpdate (symbol, scope), k ->
          Effect.continue k (Effect.perform <| Scope.AwaitUpdate (symbol, scope))
      | effect Inference.Var.AwaitUpdate var, k ->
          Effect.continue k (Effect.perform <| Inference.Var.AwaitUpdate var)
      (* if symbol.name = "TTT" then println "TTT handled properly2";
        scope.on_update <- (fun () -> Effect.continue k true) :: scope.on_update *));

  (* let result = Substitute_bindings.sub_ty ~state result in *)
  result

and enter_scope ?(new_result_scope : bool = false) ~span ~(recursive : bool)
    (state : state) : state =
  let result_scope : VarScope.t =
    if new_result_scope then
      Some
        {
          id = Id.gen ();
          depth = state.scope.depth + 1;
          span;
          parent = state.result_scope;
          locals = Scope.Locals.empty;
          recursive;
          closed = false;
          on_update = [];
        }
    else state.result_scope
  in
  {
    state with
    result_scope;
    scope =
      {
        id = Id.gen ();
        depth = state.scope.depth + 1;
        span;
        parent = Some state.scope;
        locals = Scope.Locals.empty;
        recursive;
        closed = false;
        on_update = [];
      };
  }

and fork (f : unit -> unit) : unit =
  Kast_inference_base.fork (fun () ->
      try f () with
      | effect AwaitCompiled f, k ->
          let k = dont_leak_please k in
          f.on_compiled <- k.continue :: f.on_compiled
      | effect AwaitCompiledTyExpr expr, k ->
          let k = dont_leak_please k in
          expr.on_compiled <- k.continue :: expr.on_compiled
      | effect Scope.AwaitUpdate (name, scope), k ->
          let k = dont_leak_please k in
          scope.on_update <-
            (name, fun () -> k.continue true) :: scope.on_update
      | effect Inference.Var.AwaitUpdate var, k ->
          let k = dont_leak_please k in
          Inference.Var.once_inferred (fun _ -> k.continue true) var)

let () =
  Substitute_bindings.Impl.Interpreter.instantiate := Some instantiate;
  Substitute_bindings.Impl.Interpreter.get_field := Some get_field;
  Substitute_bindings.Impl.Interpreter.claim_ref := Some claim_ref
