open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
module Inference = Kast_inference

type signature = ir_signature

let init_evaled () : Types.ir_evaled =
  { exprs = []
  ; ty_exprs = []
  ; patterns = []
  ; ty_ascribed = false
  ; value = None
  ; binding = None
  }
;;

let tuple_sig
  :  'a.
     scope:VarScope.t
  -> span:span
  -> 'a compiled_kind
  -> 'a Types.tuple_of
  -> signature
  =
  fun (type a)
    ~scope
    ~span
    (kind : a compiled_kind)
    ({ guaranteed_anonymous; parts } : a Types.tuple_of)
    : signature ->
  let result_ty = Ty.new_not_inferred ~scope ~span in
  let required_contexts = Contexts.new_not_inferred ~scope ~span in
  let produced_contexts = Contexts.new_not_inferred ~scope ~span in
  let should_infer_unpack_parts_based_on_result =
    match kind with
    | Expr -> false
    | PlaceExpr -> false
    | TyExpr -> false
    | Assignee -> true
    | Pattern -> true
  in
  if should_infer_unpack_parts_based_on_result
  then
    result_ty.var
    |> Inference.Var.once_inferred (fun shape ->
      let field_parts_unnamed_before_packed = ref 0 in
      let field_parts_unnamed = ref 0 in
      let field_parts_named = ref StringSet.empty in
      let unpacked_parts = ref [] in
      parts
      |> List.iter (fun part ->
        match (part : a Types.tuple_part_of) with
        | Field { label; label_span = _; field = _ } ->
          (match label with
           | None ->
             field_parts_unnamed := !field_parts_unnamed + 1;
             if !unpacked_parts = []
             then
               field_parts_unnamed_before_packed := !field_parts_unnamed_before_packed + 1
           | Some label ->
             field_parts_named
             := !field_parts_named |> StringSet.add (Label.get_name label))
        | Unpack packed -> unpacked_parts := packed :: !unpacked_parts);
      match !unpacked_parts with
      | [ packed ] ->
        (match shape |> Ty.Shape.expect_tuple with
         | None -> Error.error span "expected a tuple"
         | Some tuple ->
           let packed_ty_tuple = ref Tuple.empty in
           let total_unnamed =
             tuple.tuple
             |> Tuple.to_seq
             |> Seq.filter (fun (member, _) ->
               match (member : Tuple.member) with
               | Index _ -> true
               | Name _ -> false)
             |> Seq.length
           in
           let packed_unnamed = total_unnamed - !field_parts_unnamed in
           tuple.tuple
           |> Tuple.iter (fun member field ->
             match member with
             | Index i ->
               let packed_idx = i - !field_parts_unnamed_before_packed in
               if 0 <= packed_idx && packed_idx < packed_unnamed
               then packed_ty_tuple := !packed_ty_tuple |> Tuple.add None field
             | Name name ->
               if !field_parts_named |> StringSet.contains name |> not
               then packed_ty_tuple := !packed_ty_tuple |> Tuple.add (Some name) field);
           (get_data kind packed).signature.ty
           |> Inference.Ty.expect_inferred_as
                ~span
                (Ty.inferred
                   ~span
                   (T_Tuple
                      { name = OptionalName.new_not_inferred ~scope ~span
                      ; tuple = !packed_ty_tuple
                      })))
      | [] -> ()
      | _ -> ( (* more than 1 unpack parts *) ));
  State.Scope.fork (fun () ->
    with_return (fun { return } ->
      let result_tuple = ref Tuple.empty in
      parts
      |> List.iter (fun (part : a Types.tuple_part_of) ->
        match part with
        | Field { label; label_span = _; field = (field_expr : a) } ->
          let name = label |> Option.map Label.get_name in
          let { ty = field_expr_ty } : signature = (get_data kind field_expr).signature in
          let ty_field : Types.ty_tuple_field =
            { ty = field_expr_ty; label; symbol = None }
          in
          result_tuple := !result_tuple |> Tuple.add name ty_field
        | Unpack packed ->
          let { ty = packed_ty } : signature = (get_data kind packed).signature in
          (match packed_ty |> Ty.await_inferred |> Ty.Shape.expect_tuple with
           | Some { name = _; tuple } ->
             tuple
             |> Tuple.iter (fun member ty_field ->
               let name =
                 match member with
                 | Index _ -> None
                 | Name name -> Some name
               in
               result_tuple := !result_tuple |> Tuple.add name ty_field)
           | None ->
             Error.error (get_data kind packed).span "packed must be tuple";
             return ()));
      let inferred =
        Ty.inferred ~span
        <| T_Tuple
             { name =
                 (if guaranteed_anonymous
                  then OptionalName.new_inferred ~span None
                  else OptionalName.new_not_inferred ~scope ~span)
             ; tuple = !result_tuple
             }
      in
      result_ty |> Inference.Ty.expect_inferred_as ~span inferred));
  { ty = result_ty }
;;

let rec _unused () = ()

and expr_placeholder : span -> State.t -> expr =
  fun span state ->
  (* TODO maybe have E_Placeholder *)
  E_Constant
    { id = Id.gen ()
    ; value = Value.new_not_inferred ~scope:(State.var_scope state) ~span
    }
  |> init_expr span state

and auto_instantiate_generics : span -> State.t -> expr -> expr =
  fun span state expr ->
  match expr.data.signature.ty.var |> Inference.Var.inferred_opt with
  | Some (T_Generic _) ->
    E_InstantiateGeneric { generic = expr; arg = expr_placeholder span state }
    |> init_expr span state
  | _ -> expr

and field_ty ~span ~state ?(obj : Types.place_expr option) ~field_span (obj_ty : ty) field
  =
  let scope = State.var_scope state in
  with_return (fun { return } ->
    let (label, member) : Label.t option * Tuple.member =
      match (field : Types.field_expr) with
      | Index i -> None, Index i
      | Name label -> Some label, Name (Label.get_name label)
      | Expr _e ->
        error span "todo expr field access";
        return <| Ty.new_not_inferred ~scope ~span:field_span
    in
    let ty = Ty.new_not_inferred ~scope ~span in
    obj_ty.var
    |> Inference.Var.once_inferred (fun (obj_shape : Ty.Shape.t) ->
      let field_ty =
        match obj_shape with
        | T_Tuple { name = _; tuple } ->
          (match Tuple.get_opt member tuple with
           | Some ty_field ->
             (let _ : Label.t option =
                Inference_impl.unite_option
                  ~span
                  (fun ~span:_ -> Label.unite)
                  label
                  ty_field.label
              in
              ());
             ty_field.ty
           | None ->
             error span "field %a is not there" Tuple.Member.print member;
             Ty.new_not_inferred ~scope ~span:field_span)
        | T_Target ->
          (match member with
           | Name "name" -> Ty.inferred ~span:field_span T_String
           | _ ->
             error span "field %a is not in target" Tuple.Member.print member;
             Ty.new_not_inferred ~scope ~span:field_span)
        | T_Ty when Option.is_some obj ->
          let module_ty = cast_as_module_ty ~span state (Option.get obj) in
          field_ty ~span ~state ~field_span module_ty field
        | other ->
          error span "%a doesnt have fields" Ty.Shape.print other;
          Ty.new_not_inferred ~scope ~span:field_span
      in
      ty |> Inference.Ty.expect_inferred_as ~span field_ty);
    ty)

and pure : ty -> signature = fun ty -> { ty }

and init_place_expr : span -> State.t -> Expr.Place.Shape.t -> Expr.Place.t =
  fun span state shape ->
  let scope = State.var_scope state in
  try
    let inferred_mut mut = IsMutable.new_inferred ~span mut in
    let mut, signature =
      match shape with
      | PE_Error -> inferred_mut true, pure (Ty.new_not_inferred ~scope ~span)
      | PE_Binding binding -> inferred_mut binding.mut, pure binding.ty
      | PE_Temp expr -> inferred_mut true, expr.data.signature
      | PE_Deref ref ->
        let mut = IsMutable.new_not_inferred ~scope ~span in
        let value_ty = Ty.new_not_inferred ~scope ~span in
        let { ty = ref_ty } : signature = ref.data.signature in
        ref_ty
        |> Inference.Ty.expect_inferred_as
             ~span
             (Ty.inferred ~span (T_Ref { mut; referenced = value_ty }));
        mut, { ty = value_ty }
      | PE_Field { obj; field; field_span } ->
        let { ty = obj_ty } : signature = obj.data.signature in
        obj.mut, { ty = field_ty ~state ~span ~obj ~field_span obj_ty field }
    in
    { shape
    ; mut
    ; data =
        { span
        ; signature
        ; evaled = init_evaled ()
        ; included_file = None
        ; id = Id.gen ()
        ; compiler_scope = state.scopes |> State.Scopes.call_site
        }
    }
  with
  | exc ->
    Log.error (fun log -> log "while initializing place expr at %a" Span.print span);
    raise exc

and cast_as_module_ty : span:span -> State.t -> Expr.Place.t -> ty =
  fun ~span state value ->
  let scope = State.var_scope state in
  let ty = Ty.new_not_inferred ~scope ~span in
  State.Scope.fork (fun () ->
    match Kast_interpreter.eval_place state.interpreter value with
    | RefBlocked _ -> Error.error span "refblocked cast_as_module_ty"
    | Place (~mut:_, place) ->
      let value = Kast_interpreter.claim ~span place in
      let module_ = Kast_interpreter.cast_as_module ~span state.interpreter value in
      ty |> Inference.Ty.expect_inferred_as ~span (Value.ty_of module_));
  ty

and cast_result_ty : span:span -> State.t -> expr -> value -> ty =
  fun ~span state value target ->
  let scope = State.var_scope state in
  let ty = Ty.new_not_inferred ~scope ~span in
  State.Scope.fork (fun () ->
    let value = Kast_interpreter.eval state.interpreter value in
    match target |> Value.await_inferred with
    | V_Generic _ ->
      let result =
        Kast_interpreter.instantiate
          ~result_ty:(Ty.inferred ~span T_Ty)
          span
          state.interpreter
          target
          value
      in
      (match result |> Value.expect_ty with
       | Some result_ty -> ty |> Inference.Ty.expect_inferred_as ~span result_ty
       | None -> Error.error span "must be a generic type")
    | other -> Error.error span "can't cast into %a" Value.Shape.print other);
  ty

and ignored_ty (_ : ty) = ( (* TODO maybe should be a warning? configurable *) )

and init_expr : span -> State.t -> Expr.Shape.t -> expr =
  fun span state shape ->
  let scope = State.var_scope state in
  try
    let overwrite_shape : Expr.Shape.t option ref = ref None in
    let signature =
      match shape with
      | E_Constant { id = _; value } -> pure (Value.ty_of value)
      | E_Ref { mut; place } ->
        if mut
        then
          place.mut.var
          |> Inference.Var.once_inferred (fun place_mut ->
            if not place_mut then Error.error span "not mutable");
        let { ty = place_ty } : signature = place.data.signature in
        { ty =
            Ty.inferred ~span
            <| T_Ref { mut = IsMutable.new_inferred ~span mut; referenced = place_ty }
        }
      | E_Claim place -> place.data.signature
      | E_Then { list } ->
        let ty =
          match List.last_opt list with
          | None -> Ty.inferred ~span T_Unit
          | Some last -> last.data.signature.ty
        in
        { ty }
      | E_Stmt { expr } ->
        let { ty = expr_ty } : signature = expr.data.signature in
        ignored_ty expr_ty;
        { ty = Ty.inferred ~span T_Unit }
      | E_Scope { expr } -> expr.data.signature
      | E_Fn { ty; _ } -> { ty = Ty.inferred ~span <| T_Fn ty }
      | E_Generic { def = _; ty } -> { ty = T_Generic ty |> Ty.inferred ~span }
      | E_InstantiateGeneric { generic; arg } ->
        let ty = Ty.new_not_inferred ~scope ~span in
        let { ty = generic_ty } : signature = generic.data.signature in
        let { ty = arg_ty } : signature = arg.data.signature in
        State.Scope.fork (fun () ->
          let inferred_ty =
            with_return (fun { return } ->
              let ({ args = { pattern = arg_pattern }; result = result_ty }
                    : Types.ty_generic)
                =
                match generic_ty |> Ty.await_inferred with
                | T_Generic ty -> ty
                | _ ->
                  Error.error span "Expected a generic";
                  return (Ty.inferred ~span T_Error)
              in
              arg_ty
              |> Inference.Ty.expect_inferred_as
                   ~span:arg.data.span
                   arg_pattern.data.signature.ty;
              let arg = Kast_interpreter.eval state.interpreter arg in
              let ~matched:arg_matched, arg_bindings =
                Kast_interpreter.pattern_match
                  ~span
                  (Place.init ~mut:Inherit arg)
                  arg_pattern
              in
              if not arg_matched then Error.error span "Failed to pattern match fn's arg";
              let sub_state =
                { state.interpreter with
                  scope =
                    Kast_interpreter.Scope.with_values
                      ~span:arg_pattern.data.span
                      ~recursive:false
                      ~parent:None
                      arg_bindings
                }
              in
              Log.trace (fun log ->
                log
                  "generic %t (before sub) result ty = %a"
                  (Span.print_osc8 span String.print "instantiation")
                  Ty.print
                  result_ty);
              result_ty
              |> Kast_interpreter.Substitute_bindings.sub_ty ~span ~state:sub_state)
          in
          Log.trace (fun log ->
            log
              "generic %t result ty = %a"
              (Span.print_osc8 span String.print "instantiation")
              Ty.print
              inferred_ty);
          ty |> Inference.Ty.expect_inferred_as ~span inferred_ty);
        { ty }
      | E_Tuple tuple -> tuple_sig ~scope ~span Expr tuple
      | E_Variant { label; label_span = _; value } ->
        { ty =
            Ty.inferred ~span
            <| T_Variant
                 { name = OptionalName.new_not_inferred ~scope ~span
                 ; variants =
                     Row.inferred (module VarScope) VarScope.of_ty_variant_data ~span
                     <| R_Cons
                          { label
                          ; value : Types.ty_variant_data =
                              { data =
                                  value
                                  |> Option.map (fun (expr : expr) ->
                                    expr.data.signature.ty)
                              }
                          ; rest = Row.new_not_inferred ~scope ~span
                          }
                 }
        }
      | E_Apply { f; arg } ->
        let f = f |> auto_instantiate_generics f.data.span state in
        overwrite_shape := Some (E_Apply { f; arg });
        let f_arg_ty = Ty.new_not_inferred ~scope ~span in
        let f_result_ty = Ty.new_not_inferred ~scope ~span in
        let { ty = f_ty } : signature = f.data.signature in
        f_ty
        |> Inference.Ty.expect_inferred_as
             ~span:f.data.span
             (Ty.inferred ~span:f.data.span
              <| T_Fn { args = { ty = f_arg_ty }; result = f_result_ty });
        let { ty = arg_ty } : signature = arg.data.signature in
        arg_ty |> Inference.Ty.expect_inferred_as ~span:arg.data.span f_arg_ty;
        { ty = f_result_ty }
      | E_Assign { assignee; value } ->
        let { ty = assignee_ty } : signature = assignee.data.signature in
        let { ty = value_ty } : signature = value.data.signature in
        value_ty |> Inference.Ty.expect_inferred_as ~span:value.data.span assignee_ty;
        { ty = Ty.inferred ~span T_Unit }
      | E_Ty _ -> { ty = Ty.inferred ~span T_Ty }
      | E_Newtype _ -> { ty = Ty.inferred ~span T_Ty }
      | E_Native _ -> { ty = Ty.new_not_inferred ~scope ~span }
      | E_Module { def; bindings = _ } ->
        let { ty = def_ty } : signature = def.data.signature in
        def_ty
        |> Inference.Ty.expect_inferred_as ~span:def.data.span (Ty.inferred ~span T_Unit);
        { ty =
            Ty.inferred
              ~span
              (T_Tuple
                 { name =
                     OptionalName.new_inferred
                       ~span
                       (Some (Kast_interpreter.current_name state.interpreter))
                 ; tuple =
                     Tuple.make
                       []
                       (state.scopes
                        |> State.Scopes.bindings
                        |> List.map (fun (name, (binding : binding)) ->
                          ( name
                          , ({ ty = binding.ty
                             ; label = Some binding.label
                             ; symbol = Some binding.name
                             }
                             : Types.ty_tuple_field) )))
                 })
        }
      | E_UseDotStar { used = _; bindings = _ } -> { ty = Ty.inferred ~span T_Unit }
      | E_If { cond; then_case; else_case } ->
        let { ty = cond_ty } : signature = cond.data.signature in
        cond_ty
        |> Inference.Ty.expect_inferred_as
             ~span:cond.data.span
             (Ty.inferred ~span:cond.data.span T_Bool);
        let ty = Ty.new_not_inferred ~scope ~span in
        let { ty = then_ty } : signature = then_case.data.signature in
        let { ty = else_ty } : signature = else_case.data.signature in
        then_ty |> Inference.Ty.expect_inferred_as ~span:then_case.data.span ty;
        else_ty |> Inference.Ty.expect_inferred_as ~span:else_case.data.span ty;
        { ty }
      | E_And { lhs; rhs } | E_Or { lhs; rhs } ->
        let { ty = lhs_ty } : signature = lhs.data.signature in
        let { ty = rhs_ty } : signature = rhs.data.signature in
        lhs_ty
        |> Inference.Ty.expect_inferred_as
             ~span:lhs.data.span
             (Ty.inferred ~span:lhs.data.span T_Bool);
        rhs_ty
        |> Inference.Ty.expect_inferred_as
             ~span:lhs.data.span
             (Ty.inferred ~span:rhs.data.span T_Bool);
        { ty = T_Bool |> Ty.inferred ~span }
      | E_Match { value; branches } ->
        let result_ty = Ty.new_not_inferred ~scope ~span in
        let { ty = value_ty } : signature = value.data.signature in
        branches
        |> List.iter (fun (branch : Types.expr_match_branch) ->
          let { ty = branch_pattern_ty } : signature = branch.pattern.data.signature in
          let { ty = branch_body_ty } : signature = branch.body.data.signature in
          branch_pattern_ty
          |> Inference.Ty.expect_inferred_as ~span:branch.pattern.data.span value_ty;
          branch_body_ty
          |> Inference.Ty.expect_inferred_as ~span:branch.body.data.span result_ty);
        { ty = result_ty }
      | E_QuoteAst _ ->
        (* TODO assert all children are ast *)
        { ty = Ty.inferred ~span T_Ast }
      | E_Loop { body } ->
        let { ty = body_ty } : signature = body.data.signature in
        ignored_ty body_ty;
        { ty = Ty.new_not_inferred ~scope ~span }
      | E_Error -> { ty = Ty.new_not_inferred ~scope ~span }
      | E_Unwindable { token; body } ->
        let { ty = token_ty } : signature = token.data.signature in
        let { ty = body_ty } : signature = body.data.signature in
        token_ty
        |> Inference.Ty.expect_inferred_as
             ~span:token.data.span
             (Ty.inferred ~span:token.data.span <| T_UnwindToken { result = body_ty });
        { ty = body_ty }
      | E_TargetDependent { branches; captured = _; interpreter_branch = _ } ->
        let result_ty = Ty.new_not_inferred ~scope ~span in
        branches
        |> List.iter (fun ({ cond; body } : Types.expr_target_dependent_branch) ->
          let { ty = cond_ty } : signature = cond.data.signature in
          cond_ty
          |> Inference.Ty.expect_inferred_as
               ~span:cond.data.span
               (Ty.inferred ~span:cond.data.span T_Bool);
          let { ty = body_ty } : signature = body.data.signature in
          result_ty |> Inference.Ty.expect_inferred_as ~span:body.data.span body_ty);
        { ty = result_ty }
      | E_InjectContext _ -> { ty = Ty.inferred ~span T_Unit }
      | E_CurrentContext { context_ty } -> { ty = context_ty.ty }
      | E_ImplCast { value; target; impl } ->
        let { ty = impl_ty } : signature = impl.data.signature in
        impl_ty
        |> Inference.Ty.expect_inferred_as ~span (cast_result_ty ~span state value target);
        { ty = Ty.inferred ~span T_Unit }
      | E_Cast { value; target } -> { ty = cast_result_ty ~span state value target }
      | E_Unwind { token; value } ->
        let { ty = token_ty } : signature = token.data.signature in
        let { ty = value_ty } : signature = value.data.signature in
        token_ty
        |> Inference.Ty.expect_inferred_as
             ~span:token.data.span
             (Ty.inferred ~span:token.data.span (T_UnwindToken { result = value_ty }));
        { ty = Ty.never ~scope ~span }
    in
    { shape = !overwrite_shape |> Option.value ~default:shape
    ; data =
        { span
        ; signature
        ; evaled = init_evaled ()
        ; included_file = None
        ; id = Id.gen ()
        ; compiler_scope = state.scopes |> State.Scopes.call_site
        }
    }
  with
  | Cancel -> raise Cancel
  | exc ->
    let backtrace = Printexc.get_raw_backtrace () in
    Log.error (fun log -> log "while initializing expr at %a" Span.print span);
    Printexc.raise_with_backtrace exc backtrace
;;

let init_assignee : span -> State.t -> Expr.Assignee.Shape.t -> Expr.assignee =
  fun span state shape ->
  let scope = State.var_scope state in
  try
    let signature : signature =
      match shape with
      | A_Placeholder -> { ty = Ty.new_not_inferred ~scope ~span }
      | A_Unit -> { ty = Ty.inferred ~span T_Unit }
      | A_Tuple tuple -> tuple_sig ~scope ~span Assignee tuple
      | A_Let pattern -> pattern.data.signature
      | A_Place place ->
        place.mut.var
        |> Inference.Var.once_inferred (fun mut ->
          if not mut then Error.error span "Not mutable");
        place.data.signature
      | A_Error -> { ty = Ty.new_not_inferred ~scope ~span }
    in
    { shape
    ; data =
        { span
        ; signature
        ; evaled = init_evaled ()
        ; included_file = None
        ; id = Id.gen ()
        ; compiler_scope = state.scopes |> State.Scopes.call_site
        }
    }
  with
  | exc ->
    Log.error (fun log -> log "while initializing assignee expr at %a" Span.print span);
    raise exc
;;

let init_pattern : span -> State.t -> Pattern.Shape.t -> pattern =
  fun span state shape ->
  let scope = State.var_scope state in
  try
    let ty =
      match shape with
      | P_Placeholder -> Ty.new_not_inferred ~scope ~span
      | P_Unit -> Ty.inferred ~span T_Unit
      | P_Ref inner ->
        Ty.inferred ~span
        <| T_Ref
             { mut = IsMutable.new_inferred ~span false
             ; referenced = inner.data.signature.ty
             }
      | P_Binding { bind_mode; binding } ->
        (match bind_mode with
         | ByRef { mut } ->
           let result = Ty.new_not_inferred ~scope ~span in
           binding.ty
           |> Inference.Ty.expect_inferred_as
                ~span
                (Ty.inferred
                   ~span
                   (T_Ref { mut = IsMutable.new_inferred ~span mut; referenced = result }));
           result
         | Claim -> binding.ty)
      | P_Tuple tuple ->
        let { ty } : signature = tuple_sig ~scope ~span Pattern tuple in
        ty
      | P_Variant { label; label_span = _; value } ->
        Ty.inferred ~span
        <| T_Variant
             { name = OptionalName.new_not_inferred ~scope ~span
             ; variants =
                 Row.inferred (module VarScope) VarScope.of_ty_variant_data ~span
                 <| R_Cons
                      { label
                      ; value : Types.ty_variant_data =
                          { data =
                              value
                              |> Option.map (fun (pattern : pattern) ->
                                pattern.data.signature.ty)
                          }
                      ; rest = Row.new_not_inferred ~scope ~span
                      }
             }
      | P_Error -> Ty.new_not_inferred ~scope ~span
    in
    { shape
    ; data =
        { span
        ; signature = { ty }
        ; evaled = init_evaled ()
        ; included_file = None
        ; id = Id.gen ()
        ; compiler_scope = state.scopes |> State.Scopes.call_site
        }
    }
  with
  | exc ->
    Log.error (fun log -> log "while initializing pattern at %a" Span.print span);
    raise exc
;;

let init_ty_expr : span -> State.t -> (unit -> Expr.Ty.Shape.t) -> Expr.ty =
  fun span state shape ->
  let type_ty = Ty.inferred ~span T_Ty in
  try
    let result : Expr.ty =
      { compiled_shape = None
      ; on_compiled = []
      ; data =
          { span
          ; signature = { ty = type_ty }
          ; evaled = init_evaled ()
          ; included_file = None
          ; id = Id.gen ()
          ; compiler_scope = state.scopes |> State.Scopes.call_site
          }
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
        let { ty = expr_ty } : signature = expr.data.signature in
        expr_ty |> Inference.Ty.expect_inferred_as ~span:expr.data.span type_ty
      | TE_Tuple _ -> ()
      | TE_Union { elements = _ } -> ()
      | TE_Variant { variants = _ } -> ()
      | TE_Error -> ());
    result
  with
  | exc ->
    Log.error (fun log -> log "while initializing type expr at %a" Span.print span);
    raise exc
;;

let init_error : 'a. span -> State.t -> 'a compiled_kind -> 'a =
  fun (type a) span state (kind : a compiled_kind) : a ->
  match kind with
  | Expr -> E_Error |> init_expr span state
  | Pattern -> P_Error |> init_pattern span state
  | Assignee -> A_Error |> init_assignee span state
  | TyExpr -> (fun () -> TE_Error) |> init_ty_expr span state
  | PlaceExpr -> PE_Error |> init_place_expr span state
;;
