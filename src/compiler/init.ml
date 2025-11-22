open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
module Inference = Kast_inference

let rec init_expr :
    ?evaled_exprs:expr list -> span -> State.t -> Expr.Shape.t -> expr =
 fun ?(evaled_exprs = []) span state shape ->
  try
    let ty =
      match shape with
      | E_Constant value -> Value.ty_of value
      | E_Binding binding -> binding.ty
      | E_Then { a; b } -> b.data.ty
      | E_Stmt { expr } -> Ty.inferred T_Unit
      | E_Scope { expr } -> expr.data.ty
      | E_Fn { ty; _ } -> Ty.inferred <| T_Fn ty
      | E_Generic { def } -> T_Generic { def } |> Ty.inferred
      | E_InstantiateGeneric { generic; arg } ->
          let ty = Ty.new_not_inferred () in
          State.Scope.fork (fun () ->
              let inferred_ty =
                with_return (fun { return } ->
                    let ({ def } : Types.ty_generic) =
                      match
                        generic.data.ty.var |> Inference.Var.await_inferred
                      with
                      | T_Generic ty -> ty
                      | _ ->
                          Error.error span "Expected a generic";
                          return (Ty.inferred T_Error)
                    in
                    let arg = Kast_interpreter.eval state.interpreter arg in
                    let generic_ty_interpreter =
                      state.interpreter
                      (*TODO*)
                    in
                    let def =
                      Kast_interpreter.await_compiled ~span def
                      |> Option.unwrap_or_else (fun () ->
                          Error.error span "Generic is not compiled yet";
                          return (Ty.inferred T_Error))
                    in
                    let def_ty : Types.compiled_fn =
                      {
                        arg = def.arg;
                        body =
                          E_Constant { shape = V_Ty def.body.data.ty }
                          |> init_expr def.body.data.span state;
                        evaled_result = None;
                      }
                    in
                    let generic_ty : Types.value_untyped_fn =
                      {
                        id = Id.gen ();
                        def = { compiled = Some def_ty };
                        captured = generic_ty_interpreter.scope;
                      }
                    in
                    let result =
                      Kast_interpreter.call_untyped_fn span
                        generic_ty_interpreter generic_ty arg
                    in
                    match result |> Value.expect_ty with
                    | Some ty -> ty
                    | None ->
                        Error.error span "generic type is not a type???";
                        Ty.inferred T_Error)
              in
              ty |> Inference.Ty.expect_inferred_as ~span inferred_ty);
          ty
      | E_Tuple { tuple } ->
          Ty.inferred
          <| T_Tuple
               {
                 tuple =
                   tuple
                   |> Tuple.map
                        (fun
                          (~field_span:_, ~field_label, field_expr)
                          :
                          Types.ty_tuple_field
                        ->
                          let field_expr : expr = field_expr in
                          { ty = field_expr.data.ty; label = field_label });
               }
      | E_Variant { label; label_span = _; value } ->
          Ty.inferred
          <| T_Variant
               {
                 variants =
                   Row.inferred
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
                          rest = Row.new_not_inferred ();
                        };
               }
      | E_Apply { f; arg } ->
          let f_arg = Ty.new_not_inferred () in
          let f_result = Ty.new_not_inferred () in
          f.data.ty
          |> Inference.Ty.expect_inferred_as ~span:f.data.span
               (Ty.inferred <| T_Fn { arg = f_arg; result = f_result });
          arg.data.ty
          |> Inference.Ty.expect_inferred_as ~span:arg.data.span f_arg;
          f_result
      | E_Assign { assignee; value } ->
          value.data.ty
          |> Inference.Ty.expect_inferred_as ~span:value.data.span
               assignee.data.ty;
          Ty.inferred T_Unit
      | E_Ty _ -> Ty.inferred T_Ty
      | E_Native _ -> Ty.new_not_inferred ()
      | E_Module { def } ->
          def.data.ty
          |> Inference.Ty.expect_inferred_as ~span:def.data.span
               (Ty.inferred T_Unit);
          Ty.inferred
            (T_Tuple
               {
                 tuple =
                   Tuple.make []
                     (state.scope.bindings |> StringMap.to_list
                     |> List.map (fun (name, (binding : binding)) ->
                         ( name,
                           ({ ty = binding.ty; label = binding.label }
                             : Types.ty_tuple_field) )));
               })
      | E_Field { obj; field; field_span = _; label } ->
          let ty = Ty.new_not_inferred () in
          obj.data.ty.var
          |> Inference.Var.once_inferred (fun (obj_shape : Ty.Shape.t) ->
              let field_ty =
                match obj_shape with
                | T_Tuple { tuple } -> (
                    match Tuple.get_named_opt field tuple with
                    | Some ty_field ->
                        ignore <| Label.unite label ty_field.label;
                        ty_field.ty
                    | None ->
                        error span "field %a is not there"
                          String.print_maybe_escaped field;
                        Ty.new_not_inferred ())
                | T_Target -> (
                    match field with
                    | "name" -> Ty.inferred T_String
                    | _ ->
                        error span "field %a is not in target"
                          String.print_maybe_escaped field;
                        Ty.new_not_inferred ())
                | other ->
                    error obj.data.span "%a doesnt have fields" Ty.Shape.print
                      other;
                    Ty.new_not_inferred ()
              in
              ty |> Inference.Ty.expect_inferred_as ~span field_ty);
          ty
      | E_UseDotStar { used = _; bindings = _ } -> Ty.inferred T_Unit
      | E_If { cond; then_case; else_case } ->
          cond.data.ty
          |> Inference.Ty.expect_inferred_as ~span:cond.data.span
               (Ty.inferred T_Bool);
          let ty = Ty.new_not_inferred () in
          then_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:then_case.data.span ty;
          else_case.data.ty
          |> Inference.Ty.expect_inferred_as ~span:else_case.data.span ty;
          ty
      | E_Match { value; branches } ->
          let result_ty = Ty.new_not_inferred () in
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
          Ty.inferred T_Ast
      | E_Loop { body } -> Ty.new_not_inferred ()
      | E_Error -> Ty.new_not_inferred ()
      | E_Unwindable { token; body } ->
          token.data.ty
          |> Inference.Ty.expect_inferred_as ~span:token.data.span
               (Ty.inferred (T_UnwindToken { result = body.data.ty }));
          body.data.ty
      | E_TargetDependent { branches } ->
          let result = Ty.new_not_inferred () in
          branches
          |> List.iter
               (fun ({ cond; body } : Types.expr_target_dependent_branch) ->
                 cond.data.ty
                 |> Inference.Ty.expect_inferred_as ~span:cond.data.span
                      (Ty.inferred T_Bool);
                 result
                 |> Inference.Ty.expect_inferred_as ~span:body.data.span
                      body.data.ty);
          result
      | E_InjectContext _ -> Ty.inferred T_Unit
      | E_CurrentContext { context_ty } -> context_ty.ty
      | E_Unwind { token; value } ->
          token.data.ty
          |> Inference.Ty.expect_inferred_as ~span:token.data.span
               (Ty.inferred (T_UnwindToken { result = value.data.ty }));
          Ty.never ()
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
      | A_Placeholder -> Ty.new_not_inferred ()
      | A_Unit -> Ty.inferred T_Unit
      | A_Binding binding -> binding.ty
      | A_Let pattern -> pattern.data.ty
      | A_Error -> Ty.new_not_inferred ()
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
      | P_Placeholder -> Ty.new_not_inferred ()
      | P_Unit -> Ty.inferred T_Unit
      | P_Binding binding -> binding.ty
      | P_Tuple { tuple } ->
          Ty.inferred
            (T_Tuple
               {
                 tuple =
                   tuple
                   |> Tuple.map
                        (fun
                          (~field_span:_, ~field_label, field_pattern)
                          :
                          Types.ty_tuple_field
                        ->
                          let field_pattern : pattern = field_pattern in
                          { ty = field_pattern.data.ty; label = field_label });
               })
      | P_Variant { label; label_span = _; value } ->
          Ty.inferred
          <| T_Variant
               {
                 variants =
                   Row.inferred
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
                          rest = Row.new_not_inferred ();
                        };
               }
      | P_Error -> Ty.new_not_inferred ()
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
    ?evaled_exprs:expr list -> span -> State.t -> Expr.Ty.Shape.t -> Expr.ty =
  let type_ty = Ty.inferred T_Ty in
  fun ?(evaled_exprs = []) span state shape ->
    try
      (match shape with
      | TE_Unit -> ()
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
      {
        shape;
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
  | TyExpr -> TE_Error |> init_ty_expr span state
