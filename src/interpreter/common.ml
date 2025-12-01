open Std
open Kast_util
open Kast_types
module Ast = Kast_ast
module Error = Error
module Inference = Kast_inference

type state = Types.interpreter_state
type _ Effect.t += AwaitCompiled : Types.maybe_compiled_fn -> unit Effect.t
type _ Effect.t += AwaitCompiledTyExpr : Expr.ty -> unit Effect.t

exception
  Unwind of {
    token : Types.value_unwind_token;
    value : value;
  }

let rec pattern_match : value -> pattern -> (matched:bool * Scope.locals) =
 fun value pattern ->
  match pattern.shape with
  | P_Placeholder -> (~matched:true, Scope.Locals.empty)
  | P_Unit ->
      (* TODO assert that value is unit *)
      (~matched:true, Scope.Locals.empty)
  | P_Binding binding ->
      ( ~matched:true,
        {
          by_symbol =
            SymbolMap.singleton binding.name
              ({
                 value;
                 ty_field = { ty = Value.ty_of value; label = binding.label };
               }
                : Types.interpreter_local);
        } )
  | P_Tuple { tuple = tuple_pattern } -> (
      match value |> Value.await_inferred with
      | V_Tuple { tuple } ->
          with_return (fun { return } : (matched:bool * Scope.locals) ->
              let ~matched, matches =
                (try Tuple.zip_order_a tuple_pattern tuple
                 with Invalid_argument _ ->
                   Error.error pattern.data.span
                     "Tuple has different set of fields";
                   return (~matched:false, Scope.Locals.empty))
                |> Tuple.to_seq
                |> Seq.fold_left
                     (fun (~matched, acc)
                          (_member, (field_pattern, field_value)) ->
                       let { label_span = _; label = _; field = field_pattern }
                           : pattern Types.tuple_field_of =
                         field_pattern
                       in
                       let field_value : Types.value_tuple_field =
                         field_value
                       in
                       let ~matched:field_matched, field_matches =
                         pattern_match field_value.value field_pattern
                       in
                       ( ~matched:(matched && field_matched),
                         SymbolMap.union
                           (fun symbol _a b ->
                             Error.error pattern.data.span
                               "multiple bindings of same symbol %a"
                               Symbol.print symbol;
                             Some b)
                           acc field_matches.by_symbol ))
                     (~matched:true, SymbolMap.empty)
              in
              (~matched, { by_symbol = matches }))
      | _ ->
          Error.error pattern.data.span "Expected tuple, got %a" Value.print
            value;
          (~matched:false, Scope.Locals.empty))
  | P_Variant { label = patern_label; label_span = _; value = value_pattern }
    -> (
      match value |> Value.await_inferred with
      | V_Variant { label; data; ty = _ } ->
          if Label.same label patern_label then
            match (data, value_pattern) with
            | None, None -> (~matched:true, Scope.Locals.empty)
            | None, Some _ ->
                Error.error pattern.data.span "Expected value, got no value";
                (~matched:false, Scope.Locals.empty)
            | Some _, None ->
                Error.error pattern.data.span "Expected no value, got value";
                (~matched:false, Scope.Locals.empty)
            | Some value, Some pattern -> pattern_match value pattern
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
      let ~matched:arg_matched, arg_bindings = pattern_match arg def.arg in
      if not arg_matched then
        Error.error span "Failed to pattern match fn's arg";
      let new_state =
        {
          state with
          scope =
            Scope.with_values ~recursive:false ~parent:(Some fn.captured)
              arg_bindings;
        }
      in
      let result = eval new_state def.body in
      Log.trace (fun log ->
          log "evaled call (before sub) at %a = %a" Span.print span Value.print
            result);
      let result =
        match sub_mode with
        | None -> result
        | Full -> Substitute_bindings.sub_value ~span ~state:new_state result
        | FnOnly -> (
            match result |> Value.expect_fn with
            | Some f ->
                Substitute_bindings.sub_value ~span ~state:new_state result
            | None -> result)
        | TyOnly ->
            Value.inferred ~span
              (V_Ty
                 (match result |> Value.expect_ty with
                 | Some ty ->
                     Substitute_bindings.sub_ty ~span ~state:new_state ty
                 | None ->
                     Error.error span "Expected ty, got smth else";
                     Ty.inferred ~span T_Error))
      in
      Log.trace (fun log ->
          log "evaled call (after sub) at %a = %a" Span.print span Value.print
            result);
      result)

and instantiate (span : span) (state : state) (generic : value) (arg : value) :
    value =
  match generic |> Value.await_inferred with
  | V_Generic { id; fn } -> (
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
          Log.trace (fun log ->
              log "Instantiating generic with arg=%a at %a" Value.print arg
                Span.print span);
          let placeholder = Value.new_not_inferred ~span in
          save placeholder;
          fork (fun () ->
              try
                let evaluated_result =
                  call_untyped_fn ~sub_mode:FnOnly span state fn arg
                in
                Log.trace (fun log ->
                    log
                      "Instantiated generic with arg=%a, result=%a, uniting with=%a"
                      Value.print arg Value.print evaluated_result Value.print
                      placeholder);
                placeholder
                |> Inference.Value.expect_inferred_as ~span evaluated_result;
                Log.trace (fun log ->
                    log "After uniting result=%a" Value.print placeholder)
              with effect Inference.Var.AwaitUpdate var, k ->
                Effect.continue k
                  (Effect.perform <| Inference.Var.AwaitUpdate var));
          placeholder
      | Some result ->
          Log.trace (fun log ->
              log "Using memoized generic instantiation with arg=%a at %a = %a"
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
                  |> Inference.Var.once_inferred (fun (ty_shape : Ty.Shape.t) ->
                      Log.trace (fun log ->
                          log "Memoized generic instantiation inferred as ty=%a"
                            Ty.Shape.print ty_shape))
              | _ -> ());
          result)
  | V_Error -> V_Error |> Value.inferred ~span
  | _ ->
      Error.error span "expected generic";
      V_Error |> Value.inferred ~span

and call (span : span) (state : state) (f : value) (arg : value) : value =
  match f |> Value.await_inferred with
  | V_Fn { fn; ty = _ } -> call_untyped_fn ~sub_mode:None span state fn arg
  | V_NativeFn f -> f.impl ~caller:span ~state arg
  | V_Error -> V_Error |> Value.inferred ~span
  | _ ->
      Error.error span "expected fn";
      V_Error |> Value.inferred ~span

and assign : state -> Expr.assignee -> value -> unit =
 fun state assignee value ->
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit ->
      (* TODO assert that value is unit 🦄 *)
      ()
  | A_Binding { id = _; name; ty = _; span = _; label = _ } ->
      state.scope
      |> Scope.assign_to_existing ~span:assignee.data.span name value
  | A_Let pattern ->
      let ~matched, new_bindings = pattern_match value pattern in
      if not matched then
        Error.error assignee.data.span "Failed to pattern match";
      state.scope |> Scope.add_locals new_bindings
  | A_Error -> ()

and eval : state -> expr -> value =
 fun state expr ->
  try
    let span = expr.data.span in
    Log.trace (fun log -> log "evaluating at %a" Span.print span);
    let result =
      match expr.shape with
      | E_Constant value -> value
      | E_Binding binding ->
          let result =
            Scope.find_opt binding.name state.scope
            |> Option.unwrap_or_else (fun () : value ->
                Log.trace (fun log ->
                    log "all in scope: %a" Scope.print_all state.scope);
                Error.error expr.data.span "%a not found" Symbol.print
                  binding.name;
                V_Error |> Value.inferred ~span)
          in
          Log.trace (fun log ->
              log "evaled binding %a = %a" Binding.print binding Value.print
                result);
          result
      | E_Fn { def; ty } ->
          V_Fn { ty; fn = { id = Id.gen (); def; captured = state.scope } }
          |> Value.inferred ~span
      | E_Generic { def } ->
          V_Generic
            {
              id = Id.gen ();
              fn = { id = Id.gen (); def; captured = state.scope };
            }
          |> Value.inferred ~span
      | E_Tuple { tuple } ->
          (*  TODO dont panic - get rid of Option.get *)
          let ty =
            expr.data.ty.var |> Kast_inference_base.Var.inferred_opt
            |> Option.get |> Ty.Shape.expect_tuple |> Option.get
          in
          V_Tuple
            {
              tuple =
                tuple
                |> Tuple.mapi
                     (fun
                       member
                       ({ label_span; label = _; field = field_expr } :
                         expr Types.tuple_field_of)
                       :
                       Types.value_tuple_field
                     ->
                       let value = field_expr |> eval state in
                       let ty_field = ty.tuple |> Tuple.get member in
                       { value; span = label_span; ty_field });
            }
          |> Value.inferred ~span
      | E_Variant { label; label_span = _; value } ->
          let value = value |> Option.map (eval state) in
          V_Variant { label; data = value; ty = expr.data.ty }
          |> Value.inferred ~span
      | E_Then { a; b } ->
          ignore <| eval state a;
          eval state b
      | E_Stmt { expr } ->
          ignore <| eval state expr;
          V_Unit |> Value.inferred ~span
      | E_Scope { expr } ->
          let state = state |> enter_scope ~recursive:false in
          eval state expr
      | E_Assign { assignee; value } ->
          let value = eval state value in
          assign state assignee value;
          V_Unit |> Value.inferred ~span
      | E_Apply { f; arg } ->
          let f = eval state f in
          let arg = eval state arg in
          call expr.data.span state f arg
      | E_InstantiateGeneric { generic; arg } ->
          let generic = eval state generic in
          let arg = eval state arg in
          instantiate expr.data.span state generic arg
      | E_Ty expr -> V_Ty (eval_ty state expr) |> Value.inferred ~span
      | E_Native { expr = native_expr } -> (
          match StringMap.find_opt native_expr state.natives.by_name with
          | Some value -> value
          | None ->
              Error.error expr.data.span "no native %S" native_expr;
              V_Error |> Value.inferred ~span)
      | E_Module { def } ->
          let module_scope =
            Scope.init ~recursive:true ~parent:(Some state.scope)
          in
          let new_state = { state with scope = module_scope } in
          ignore @@ eval new_state def;
          Scope.close module_scope;
          let fields =
            module_scope.locals.by_symbol |> SymbolMap.to_list
            |> List.map
                 (fun ((symbol : symbol), (local : Types.interpreter_local)) ->
                   ( Some symbol.name,
                     ({
                        value = local.value;
                        span = local.ty_field.label |> Label.get_span;
                        ty_field = local.ty_field;
                      }
                       : Types.value_tuple_field) ))
          in
          V_Tuple { tuple = fields |> Tuple.of_list } |> Value.inferred ~span
      | E_Field { obj; field; field_span = _; label = _ } -> (
          let obj = eval state obj in
          match obj |> Value.await_inferred with
          | V_Tuple { tuple } -> (
              match Tuple.get_named_opt field tuple with
              | Some field -> field.value
              | None ->
                  Error.error expr.data.span "field %S doesnt exist" field;
                  V_Error |> Value.inferred ~span)
          | V_Target { name } -> (
              match field with
              | "name" -> V_String name |> Value.inferred ~span
              | _ ->
                  Error.error expr.data.span "field %S doesnt exist in target"
                    field;
                  V_Error |> Value.inferred ~span)
          | V_Error -> V_Error |> Value.inferred ~span
          | _ ->
              Error.error expr.data.span "%a doesnt have fields" Value.print obj;
              V_Error |> Value.inferred ~span)
      | E_UseDotStar { used; bindings } -> (
          let used = eval state used in
          match used |> Value.await_inferred with
          | V_Tuple { tuple } ->
              bindings
              |> List.iter (fun (binding : binding) ->
                  let field = tuple |> Tuple.get_named binding.name.name in
                  state.scope
                  |> Scope.add_local field.span binding.name field.value);
              V_Unit |> Value.inferred ~span
          | _ ->
              Error.error expr.data.span "can't use .* %a" Value.print used;
              V_Error |> Value.inferred ~span)
      | E_If { cond = cond_expr; then_case; else_case } -> (
          let cond = eval state cond_expr in
          match cond |> Value.await_inferred with
          | V_Bool true -> eval state then_case
          | V_Bool false -> eval state else_case
          | _ ->
              Error.error cond_expr.data.span "if cond must be bool, got %a"
                Value.print cond;
              V_Error |> Value.inferred ~span)
      | E_Match { value; branches } -> (
          let value = eval state value in
          let result =
            branches
            |> List.find_map (fun (branch : Types.expr_match_branch) ->
                let ~matched, matches = pattern_match value branch.pattern in
                if matched then
                  Some
                    (let inner_state = enter_scope ~recursive:false state in
                     inner_state.scope |> Scope.add_locals matches;
                     eval inner_state branch.body)
                else None)
          in
          match result with
          | Some result -> result
          | None ->
              Error.error expr.data.span "pattern match non exhaustive";
              V_Error |> Value.inferred ~span)
      | E_QuoteAst expr ->
          V_Ast (quote_ast ~span state expr) |> Value.inferred ~span
      | E_Loop { body } ->
          let state = state |> enter_scope ~recursive:false in
          while true do
            ignore @@ eval state body
          done
      | E_Error -> V_Error |> Value.inferred ~span
      | E_Unwindable { token = token_pattern; body } -> (
          let id = Id.gen () in
          let token : Types.value_unwind_token =
            { id; result_ty = body.data.ty }
          in
          let token : value =
            V_UnwindToken token |> Value.inferred ~span:token_pattern.data.span
          in
          let inner_state = enter_scope ~recursive:false state in
          inner_state.scope
          |> Scope.add_locals
               ( pattern_match token token_pattern |> fun (~matched, locals) ->
                 if not matched then
                   Error.error token_pattern.data.span
                     "Failed to pattern match unwind token";
                 locals );
          try eval inner_state body
          with Unwind { token; value } when token.id = id -> value)
      | E_Unwind { token = token_expr; value } ->
          with_return (fun { return } ->
              let token =
                eval state token_expr |> Value.expect_unwind_token
                |> Option.unwrap_or_else (fun () ->
                    Error.error token_expr.data.span
                      "Unwind token was incorrect type";
                    return (V_Error |> Value.inferred ~span : value))
              in
              let value = eval state value in
              raise <| Unwind { token; value })
      | E_InjectContext { context_ty; value } ->
          let value = eval state value in
          state.contexts <- state.contexts |> Id.Map.add context_ty.id value;
          V_Unit |> Value.inferred ~span
      | E_CurrentContext { context_ty } -> (
          match state.contexts |> Id.Map.find_opt context_ty.id with
          | Some value -> value
          | None ->
              Error.error expr.data.span "Context unavailable";
              V_Error |> Value.inferred ~span)
      | E_TargetDependent { branches } ->
          with_return (fun { return } ->
              let chosen_branch =
                find_target_dependent_branch state branches
                  { name = "interpreter" }
                |> Option.unwrap_or_else (fun () ->
                    Error.error expr.data.span
                      "No target dependent branch matched";
                    return (V_Error |> Value.inferred ~span : value))
              in
              eval state chosen_branch.body)
    in
    (* let result = Substitute_bindings.sub_value ~state result in *)
    Log.trace (fun log ->
        log "evaled at %a = %a" Span.print span Value.print result);
    result
  with
  | Unwind _ as exc -> raise exc
  | exc ->
      Log.error (fun log ->
          log "While evaluating %a expr at %a" Expr.print_short expr Span.print
            expr.data.span);
      raise exc

and find_target_dependent_branch :
    state ->
    Types.expr_target_dependent_branch list ->
    Types.value_target ->
    Types.expr_target_dependent_branch option =
 fun state branches target ->
  branches
  |> List.find_map (fun (branch : Types.expr_target_dependent_branch) ->
      let scope_with_target =
        Scope.init ~recursive:false ~parent:(Some state.scope)
      in
      scope_with_target
      |> Scope.add_local (Span.of_ocaml __POS__) Types.target_symbol
           (V_Target target |> Value.inferred ~span:(Span.of_ocaml __POS__));
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

and eval_ty : state -> Expr.ty -> ty =
 fun state expr ->
  let span = expr.data.span in
  let result = Ty.new_not_inferred ~span in
  fork (fun () ->
      try
        Log.trace (fun log -> log "started eval ty expr at %a" Span.print span);
        let evaled_result =
          match await_compiled_ty_expr ~span:expr.data.span expr with
          | TE_Unit -> Ty.inferred ~span T_Unit
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
          | TE_Tuple { tuple } ->
              Ty.inferred ~span
              <| T_Tuple
                   {
                     tuple =
                       tuple
                       |> Tuple.map
                            (fun
                              ({ label_span = _; label; field = field_expr } :
                                Expr.ty Types.tuple_field_of)
                              :
                              Types.ty_tuple_field
                            ->
                              let ty = field_expr |> eval_ty state in
                              { ty; label });
                   }
          | TE_Union { elements } ->
              let variants =
                elements
                |> List.map (fun ty_expr ->
                    let ty = eval_ty state ty_expr in
                    match ty |> Ty.await_inferred with
                    | T_Variant { variants } ->
                        Row.await_inferred_to_list variants
                    | _ ->
                        Error.error ty_expr.data.span
                          "Can only use variants in union";
                        [])
                |> List.flatten
              in
              Ty.inferred ~span
              <| T_Variant { variants = Row.of_list ~span variants }
          | TE_Variant { variants } ->
              Ty.inferred ~span
              <| T_Variant
                   {
                     variants =
                       Row.of_list ~span
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
              evaled_result)
      with
      | effect Scope.AwaitUpdate (symbol, scope), k ->
          Effect.continue k (Effect.perform <| Scope.AwaitUpdate (symbol, scope))
      | effect Inference.Var.AwaitUpdate var, k ->
          Effect.continue k (Effect.perform <| Inference.Var.AwaitUpdate var)
      (* if symbol.name = "TTT" then println "TTT handled properly2";
        scope.on_update <- (fun () -> Effect.continue k true) :: scope.on_update *));

  (* let result = Substitute_bindings.sub_ty ~state result in *)
  result

and enter_scope ~(recursive : bool) (state : state) : state =
  {
    state with
    scope =
      {
        id = Id.gen ();
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
          f.on_compiled <- (fun () -> Effect.continue k ()) :: f.on_compiled
      | effect AwaitCompiledTyExpr expr, k ->
          expr.on_compiled <-
            (fun () -> Effect.continue k ()) :: expr.on_compiled
      | effect Scope.AwaitUpdate (name, scope), k ->
          scope.on_update <-
            (fun () -> Effect.continue k true) :: scope.on_update
      | effect Inference.Var.AwaitUpdate var, k ->
          Inference.Var.once_inferred (fun _ -> Effect.continue k true) var)
