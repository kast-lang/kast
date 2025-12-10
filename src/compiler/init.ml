open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
module Inference = Kast_inference

let rec _unused () = ()

and expr_placeholder : span -> State.t -> expr =
 fun span state ->
  (* TODO maybe have E_Placeholder *)
  E_Constant (Value.new_not_inferred ~span) |> init_expr span state

and auto_instantiate_generics : span -> State.t -> expr -> expr =
 fun span state expr ->
  match expr.data.ty.var |> Inference.Var.inferred_opt with
  | Some (T_Generic _) ->
      E_InstantiateGeneric { generic = expr; arg = expr_placeholder span state }
      |> init_expr span state
  | _ -> expr

and init_place_expr :
    ?evaled_exprs:expr list ->
    span ->
    State.t ->
    Expr.Place.Shape.t ->
    Expr.Place.t =
 fun ?(evaled_exprs = []) span state shape ->
  try
    let ty =
      match shape with
      | PE_Error -> Ty.new_not_inferred ~span
      | PE_Binding binding -> binding.ty
      | PE_Temp expr -> expr.data.ty
      | PE_Deref ref ->
          let value_ty = Ty.new_not_inferred ~span in
          ref.data.ty
          |> Inference.Ty.expect_inferred_as ~span
               (Ty.inferred ~span (T_Ref value_ty));
          value_ty
      | PE_Field { obj; field; field_span; label } ->
          let ty = Ty.new_not_inferred ~span in
          obj.data.ty.var
          |> Inference.Var.once_inferred (fun (obj_shape : Ty.Shape.t) ->
              let field_ty =
                match obj_shape with
                | T_Tuple { name = _; tuple } -> (
                    match Tuple.get_named_opt field tuple with
                    | Some ty_field ->
                        ignore <| Label.unite label ty_field.label;
                        ty_field.ty
                    | None ->
                        error span "field %a is not there"
                          String.print_maybe_escaped field;
                        Ty.new_not_inferred ~span:field_span)
                | T_Target -> (
                    match field with
                    | "name" -> Ty.inferred ~span:field_span T_String
                    | _ ->
                        error span "field %a is not in target"
                          String.print_maybe_escaped field;
                        Ty.new_not_inferred ~span:field_span)
                | other ->
                    error obj.data.span "%a doesnt have fields" Ty.Shape.print
                      other;
                    Ty.new_not_inferred ~span:field_span
              in
              ty |> Inference.Ty.expect_inferred_as ~span field_ty);
          ty
    in
    {
      shape;
      data =
        {
          span;
          ty;
          ty_ascription = None;
          evaled_exprs;
          compiler_scope = state.scope;
          included_file = None;
        };
    }
  with exc ->
    Log.error (fun log ->
        log "while initializing place expr at %a" Span.print span);
    raise exc

and cast_result_ty : span:span -> State.t -> expr -> value -> ty =
 fun ~span state value target ->
  let ty = Ty.new_not_inferred ~span in
  State.Scope.fork (fun () ->
      let value = Kast_interpreter.eval state.interpreter value in
      match target |> Value.await_inferred with
      | V_Generic _ -> (
          let result =
            Kast_interpreter.instantiate span state.interpreter target value
          in
          match result |> Value.expect_ty with
          | Some result_ty ->
              ty |> Inference.Ty.expect_inferred_as ~span result_ty
          | None -> Error.error span "must be a generic type")
      | other -> Error.error span "can't cast into %a" Value.Shape.print other);
  ty

and init_expr :
    ?evaled_exprs:expr list -> span -> State.t -> Expr.Shape.t -> expr =
 fun ?(evaled_exprs = []) span state shape ->
  try
    let overwrite_shape : Expr.Shape.t option ref = ref None in
    let ty =
      match shape with
      | E_Constant value -> Value.ty_of value
      | E_Ref place -> Ty.inferred ~span <| T_Ref place.data.ty
      | E_Claim place -> place.data.ty
      | E_Then { list } -> (
          match List.last_opt list with
          | None -> Ty.inferred ~span T_Unit
          | Some last -> last.data.ty)
      | E_Stmt { expr } -> Ty.inferred ~span T_Unit
      | E_Scope { expr } -> expr.data.ty
      | E_Fn { ty; _ } -> Ty.inferred ~span <| T_Fn ty
      | E_Generic { def } -> T_Generic { def } |> Ty.inferred ~span
      | E_InstantiateGeneric { generic; arg } ->
          let ty = Ty.new_not_inferred ~span in
          State.Scope.fork (fun () ->
              let inferred_ty =
                with_return (fun { return } ->
                    let ({ def } : Types.ty_generic) =
                      match generic.data.ty |> Ty.await_inferred with
                      | T_Generic ty -> ty
                      | _ ->
                          Error.error span "Expected a generic";
                          return (Ty.inferred ~span T_Error)
                    in
                    let def =
                      Kast_interpreter.await_compiled ~span def
                      |> Option.unwrap_or_else (fun () ->
                          Error.error span "Generic is not compiled yet";
                          return (Ty.inferred ~span T_Error))
                    in
                    let def_ty : Types.compiled_fn =
                      {
                        arg = def.arg;
                        body =
                          E_Constant
                            (V_Ty def.body.data.ty |> Value.inferred ~span)
                          |> init_expr def.body.data.span state;
                        evaled_result = None;
                      }
                    in
                    arg.data.ty
                    |> Inference.Ty.expect_inferred_as ~span:arg.data.span
                         def.arg.data.ty;
                    let arg = Kast_interpreter.eval state.interpreter arg in
                    let generic_ty_interpreter =
                      state.interpreter
                      (*TODO*)
                    in
                    let generic_ty : Types.value_untyped_fn =
                      {
                        id = Id.gen ();
                        def = { compiled = Some def_ty; on_compiled = [] };
                        captured = generic_ty_interpreter.scope;
                        calculated_natives = Hashtbl.create 0;
                      }
                    in
                    let result =
                      Kast_interpreter.call_untyped_fn ~sub_mode:TyOnly span
                        generic_ty_interpreter generic_ty arg
                    in
                    match result |> Value.expect_ty with
                    | Some ty -> ty
                    | None ->
                        Error.error span "generic type is not a type???";
                        Ty.inferred ~span T_Error)
              in
              ty |> Inference.Ty.expect_inferred_as ~span inferred_ty);
          ty
      | E_Tuple { tuple } ->
          Ty.inferred ~span
          <| T_Tuple
               {
                 name = OptionalName.new_not_inferred ~span;
                 tuple =
                   tuple
                   |> Tuple.map
                        (fun
                          ({ label_span = _; label; field = field_expr } :
                            expr Types.tuple_field_of)
                          :
                          Types.ty_tuple_field
                        ->
                          let field_expr : expr = field_expr in
                          { ty = field_expr.data.ty; label });
               }
      | E_Variant { label; label_span = _; value } ->
          Ty.inferred ~span
          <| T_Variant
               {
                 name = OptionalName.new_not_inferred ~span;
                 variants =
                   Row.inferred ~span
                   <| R_Cons
                        {
                          label;
                          value : Types.ty_variant_data =
                            {
                              data =
                                value
                                |> Option.map (fun (expr : expr) ->
                                    expr.data.ty);
                            };
                          rest = Row.new_not_inferred ~span;
                        };
               }
      | E_Apply { f; arg } ->
          let f = f |> auto_instantiate_generics f.data.span state in
          overwrite_shape := Some (E_Apply { f; arg });
          let f_arg = Ty.new_not_inferred ~span in
          let f_result = Ty.new_not_inferred ~span in
          f.data.ty
          |> Inference.Ty.expect_inferred_as ~span:f.data.span
               (Ty.inferred ~span:f.data.span
               <| T_Fn { arg = f_arg; result = f_result });
          arg.data.ty
          |> Inference.Ty.expect_inferred_as ~span:arg.data.span f_arg;
          f_result
      | E_Assign { assignee; value } ->
          value.data.ty
          |> Inference.Ty.expect_inferred_as ~span:value.data.span
               assignee.data.ty;
          Ty.inferred ~span T_Unit
      | E_Ty _ -> Ty.inferred ~span T_Ty
      | E_Native _ -> Ty.new_not_inferred ~span
      | E_Module { def } ->
          def.data.ty
          |> Inference.Ty.expect_inferred_as ~span:def.data.span
               (Ty.inferred ~span T_Unit);
          Ty.inferred ~span
            (T_Tuple
               {
                 name = Kast_interpreter.current_optional_name state.interpreter;
                 tuple =
                   Tuple.make []
                     (state.scope.bindings |> StringMap.to_list
                     |> List.map (fun (name, (binding : binding)) ->
                         ( name,
                           ({ ty = binding.ty; label = binding.label }
                             : Types.ty_tuple_field) )));
               })
      | E_UseDotStar { used = _; bindings = _ } -> Ty.inferred ~span T_Unit
      | E_If { cond; then_case; else_case } ->
          cond.data.ty
          |> Inference.Ty.expect_inferred_as ~span:cond.data.span
               (Ty.inferred ~span:cond.data.span T_Bool);
          let ty = Ty.new_not_inferred ~span in
          then_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:then_case.data.span ty;
          else_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:else_case.data.span ty;
          ty
      | E_And { lhs; rhs } | E_Or { lhs; rhs } ->
          lhs.data.ty
          |> Inference.Ty.expect_inferred_as ~span:lhs.data.span
               (Ty.inferred ~span:lhs.data.span T_Bool);
          rhs.data.ty
          |> Inference.Ty.expect_inferred_as ~span:lhs.data.span
               (Ty.inferred ~span:rhs.data.span T_Bool);
          T_Bool |> Ty.inferred ~span
      | E_Match { value; branches } ->
          let result_ty = Ty.new_not_inferred ~span in
          let value_ty = value.data.ty in
          branches
          |> List.iter (fun (branch : Types.expr_match_branch) ->
              branch.pattern.data.ty
              |> Inference.Ty.expect_inferred_as ~span:branch.pattern.data.span
                   value_ty;
              branch.body.data.ty
              |> Inference.Ty.expect_inferred_as ~span:branch.body.data.span
                   result_ty);
          result_ty
      | E_QuoteAst _ ->
          (* TODO assert all children are ast *)
          Ty.inferred ~span T_Ast
      | E_Loop { body } -> Ty.new_not_inferred ~span
      | E_Error -> Ty.new_not_inferred ~span
      | E_Unwindable { token; body } ->
          token.data.ty
          |> Inference.Ty.expect_inferred_as ~span:token.data.span
               (Ty.inferred ~span:token.data.span
               <| T_UnwindToken { result = body.data.ty });
          body.data.ty
      | E_TargetDependent { branches; interpreter_branch = _ } ->
          let result = Ty.new_not_inferred ~span in
          branches
          |> List.iter
               (fun ({ cond; body } : Types.expr_target_dependent_branch) ->
                 cond.data.ty
                 |> Inference.Ty.expect_inferred_as ~span:cond.data.span
                      (Ty.inferred ~span:cond.data.span T_Bool);
                 result
                 |> Inference.Ty.expect_inferred_as ~span:body.data.span
                      body.data.ty);
          result
      | E_InjectContext _ -> Ty.inferred ~span T_Unit
      | E_CurrentContext { context_ty } -> context_ty.ty
      | E_ImplCast { value; target; impl } ->
          impl.data.ty
          |> Inference.Ty.expect_inferred_as ~span
               (cast_result_ty ~span state value target);
          Ty.inferred ~span T_Unit
      | E_Cast { value; target } -> cast_result_ty ~span state value target
      | E_Unwind { token; value } ->
          token.data.ty
          |> Inference.Ty.expect_inferred_as ~span:token.data.span
               (Ty.inferred ~span:token.data.span
                  (T_UnwindToken { result = value.data.ty }));
          Ty.never ~span
    in
    {
      shape = !overwrite_shape |> Option.value ~default:shape;
      data =
        {
          span;
          ty;
          ty_ascription = None;
          evaled_exprs;
          compiler_scope = state.scope;
          included_file = None;
        };
    }
  with exc ->
    Log.error (fun log -> log "while initializing expr at %a" Span.print span);
    raise exc

let init_assignee :
    ?evaled_exprs:expr list ->
    span ->
    State.t ->
    Expr.Assignee.Shape.t ->
    Expr.assignee =
 fun ?(evaled_exprs = []) span state shape ->
  try
    let ty =
      match shape with
      | A_Placeholder -> Ty.new_not_inferred ~span
      | A_Unit -> Ty.inferred ~span T_Unit
      | A_Tuple { tuple } ->
          Ty.inferred ~span
          <| T_Tuple
               {
                 name = OptionalName.new_not_inferred ~span;
                 tuple =
                   tuple
                   |> Tuple.map
                        (fun
                          ({ label_span = _; label; field = field_expr } :
                            Expr.assignee Types.tuple_field_of)
                          :
                          Types.ty_tuple_field
                        ->
                          let field_expr : Expr.assignee = field_expr in
                          { ty = field_expr.data.ty; label });
               }
      | A_Let pattern -> pattern.data.ty
      | A_Place place -> place.data.ty
      | A_Error -> Ty.new_not_inferred ~span
    in
    {
      shape;
      data =
        {
          span;
          ty;
          ty_ascription = None;
          compiler_scope = state.scope;
          evaled_exprs;
          included_file = None;
        };
    }
  with exc ->
    Log.error (fun log ->
        log "while initializing assignee expr at %a" Span.print span);
    raise exc

let init_pattern :
    ?evaled_exprs:expr list -> span -> State.t -> Pattern.Shape.t -> pattern =
 fun ?(evaled_exprs = []) span state shape ->
  try
    let ty =
      match shape with
      | P_Placeholder -> Ty.new_not_inferred ~span
      | P_Unit -> Ty.inferred ~span T_Unit
      | P_Ref inner -> Ty.inferred ~span <| T_Ref inner.data.ty
      | P_Binding binding -> binding.ty
      | P_Tuple { tuple } ->
          Ty.inferred ~span
            (T_Tuple
               {
                 name = OptionalName.new_not_inferred ~span;
                 tuple =
                   tuple
                   |> Tuple.map
                        (fun
                          ({ label_span = _; label; field = field_pattern } :
                            pattern Types.tuple_field_of)
                          :
                          Types.ty_tuple_field
                        ->
                          let field_pattern : pattern = field_pattern in
                          { ty = field_pattern.data.ty; label });
               })
      | P_Variant { label; label_span = _; value } ->
          Ty.inferred ~span
          <| T_Variant
               {
                 name = OptionalName.new_not_inferred ~span;
                 variants =
                   Row.inferred ~span
                   <| R_Cons
                        {
                          label;
                          value : Types.ty_variant_data =
                            {
                              data =
                                value
                                |> Option.map (fun (pattern : pattern) ->
                                    pattern.data.ty);
                            };
                          rest = Row.new_not_inferred ~span;
                        };
               }
      | P_Error -> Ty.new_not_inferred ~span
    in
    {
      shape;
      data =
        {
          span;
          ty;
          ty_ascription = None;
          compiler_scope = state.scope;
          evaled_exprs;
          included_file = None;
        };
    }
  with exc ->
    Log.error (fun log ->
        log "while initializing pattern at %a" Span.print span);
    raise exc

let init_ty_expr :
    ?evaled_exprs:expr list ->
    span ->
    State.t ->
    (unit -> Expr.Ty.Shape.t) ->
    Expr.ty =
 fun ?(evaled_exprs = []) span state shape ->
  let type_ty = Ty.inferred ~span T_Ty in
  try
    let result : Expr.ty =
      {
        compiled_shape = None;
        on_compiled = [];
        data =
          {
            span;
            ty = type_ty;
            ty_ascription = None;
            compiler_scope = state.scope;
            evaled_exprs;
            included_file = None;
          };
      }
    in
    State.Scope.fork (fun () ->
        let shape = shape () in
        result.compiled_shape <- Some shape;
        let fs = result.on_compiled in
        result.on_compiled <- [];
        fs |> List.iter (fun f -> f ());
        match shape with
        | TE_Unit -> ()
        | TE_Ref _ -> ()
        | TE_Fn { arg; result } ->
            let _ : Expr.ty = arg in
            let _ : Expr.ty = result in
            ()
        | TE_Expr expr ->
            expr.data.ty
            |> Inference.Ty.expect_inferred_as ~span:expr.data.span type_ty
        | TE_Tuple { tuple = _ } -> ()
        | TE_Union { elements = _ } -> ()
        | TE_Variant { variants = _ } -> ()
        | TE_Error -> ());
    result
  with exc ->
    Log.error (fun log ->
        log "while initializing type expr at %a" Span.print span);
    raise exc

let init_error : 'a. span -> State.t -> 'a compiled_kind -> 'a =
 fun (type a) span state (kind : a compiled_kind) : a ->
  match kind with
  | Expr -> E_Error |> init_expr span state
  | Pattern -> P_Error |> init_pattern span state
  | Assignee -> A_Error |> init_assignee span state
  | TyExpr -> (fun () -> TE_Error) |> init_ty_expr span state
  | PlaceExpr -> PE_Error |> init_place_expr span state
