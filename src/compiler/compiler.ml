open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
module Ast = Kast_ast
module Inference = Kast_inference

let get_data = get_data

module CompiledKind = struct
  type 'a t = 'a compiled_kind

  let print : 'a. formatter -> 'a compiled_kind -> unit =
    fun (type a) fmt (kind : a compiled_kind) : unit ->
    match kind with
    | Assignee -> fprintf fmt "assignee expr"
    | Expr -> fprintf fmt "expr"
    | TyExpr -> fprintf fmt "type expr"
    | Pattern -> fprintf fmt "pattern"
    | PlaceExpr -> fprintf fmt "place expr"
  ;;
end

module type S = sig
  val state : State.t
  val compile : 'a. ?state:State.t -> 'a compiled_kind -> Ast.t -> 'a
end

let update_module (module C : S) state : (module S) =
  (module struct
    let state = state

    let compile ?state:state_override kind ast =
      let state = state_override |> Option.value ~default:state in
      C.compile ~state kind ast
    ;;
  end)
;;

type compiler = { compile : 'a. 'a compiled_kind -> Ast.t -> 'a }

let update_data : 'a. 'a compiled_kind -> 'a -> (ir_data -> ir_data) -> 'a =
  fun (type a) (kind : a compiled_kind) (compiled : a) (f : ir_data -> ir_data) : a ->
  match kind with
  | Expr -> { compiled with data = f compiled.data }
  | Assignee -> { compiled with data = f compiled.data }
  | TyExpr -> { compiled with data = f compiled.data }
  | Pattern -> { compiled with data = f compiled.data }
  | PlaceExpr -> { compiled with data = f compiled.data }
;;

let data_add
      (type a)
      (added_kind : a compiled_kind)
      (added : a)
      (type b)
      (kind : b compiled_kind)
      (compiled : b)
  : b
  =
  let evaled = (get_data kind compiled).evaled in
  (match added_kind with
   | TyExpr -> evaled.ty_exprs <- added :: evaled.ty_exprs
   | Expr -> evaled.exprs <- added :: evaled.exprs
   | Pattern -> evaled.patterns <- added :: evaled.patterns
   | _ -> failwith __LOC__);
  compiled
;;

let eval_ty (module C : S) (ast : Ast.t) : ty * Expr.ty =
  let ty_expr = C.compile TyExpr ast in
  let ty : ty =
    ty_expr.data.ty
    |> Inference.Ty.expect_inferred_as
         ~span:ty_expr.data.span
         (Ty.inferred ~span:ty_expr.data.span T_Ty);
    Kast_interpreter.eval_ty C.state.interpreter ty_expr
  in
  ty, ty_expr
;;

let eval ~(ty : ty) (module C : S) (ast : Ast.t) : value * expr =
  let expr = C.compile Expr ast in
  let value : value =
    expr.data.ty |> Inference.Ty.expect_inferred_as ~span:expr.data.span ty;
    Kast_interpreter.eval C.state.interpreter expr
  in
  value, expr
;;

let temp_expr (module C : S) (ast : Ast.t) : Expr.Place.t =
  let expr = C.compile Expr ast in
  PE_Temp expr |> Init.init_place_expr ast.span C.state
;;

let import ~(span : span) (module C : S) (uri : Uri.t) : value =
  let import_cache = C.state.import_cache in
  let result : State.imported =
    match UriMap.find_opt uri import_cache.by_uri with
    | None ->
      Log.trace (fun log -> log "Importing %a" Uri.print uri);
      Effect.perform (CompilerEffect.FileStartedProcessing uri);
      import_cache.by_uri
      <- UriMap.add uri (InProgress : State.import) import_cache.by_uri;
      let state : State.t =
        { currently_compiled_file = Some uri
        ; (* TODO should have impls from std? *)
          custom_syntax_impls = Hashtbl.create 0
        ; (* TODO why is this not a new scope? *)
          scope = C.state.scope
        ; import_cache
        ; interpreter =
            { (* TODO should be brand new interpreter? and compiler? *)
              C.state.interpreter
              with
              cast_impls =
                { map = Types.ValueMap.empty; as_module = Types.ValueMap.empty }
            }
        ; mut_enabled = false
        ; bind_mode = Claim
        }
      in
      let source = Source.read uri in
      let parsed = Kast_parser.parse source Kast_default_syntax.ruleset in
      let expr =
        match parsed.ast with
        | Some ast -> C.compile ~state Expr ast
        | None ->
          E_Constant (V_Unit |> Value.inferred ~span)
          |> Init.init_expr (Span.beginning_of source.uri) state
      in
      let value : value = Kast_interpreter.eval state.interpreter expr in
      Effect.perform (CompilerEffect.FileImported { uri; parsed; compiled = expr; value });
      let imported : State.imported =
        { value
        ; custom_syntax_impls = state.custom_syntax_impls
        ; cast_impls = state.interpreter.cast_impls
        }
      in
      import_cache.by_uri
      <- UriMap.add uri (Imported imported : State.import) import_cache.by_uri;
      Log.trace (fun log -> log "Imported %a" Uri.print uri);
      imported
    | Some (Imported value) -> value
    | Some InProgress ->
      error span "No recursive imports!";
      { value = V_Error |> Value.inferred ~span
      ; custom_syntax_impls = Hashtbl.create 0
      ; cast_impls = { map = Types.ValueMap.empty; as_module = Types.ValueMap.empty }
      }
  in
  Log.trace (fun log -> log "imported (maybe cached) %a" Uri.print uri);
  Hashtbl.add_seq C.state.custom_syntax_impls (Hashtbl.to_seq result.custom_syntax_impls);
  (* TODO what if its going to be evaluated in a different interpreter? *)
  result.cast_impls.map
  |> Types.ValueMap.iter (fun target impls ->
    impls
    |> Types.ValueMap.iter (fun value impl ->
      Log.trace (fun log ->
        log
          "Imported impl %a as %a = %a"
          Value.print
          value
          Value.print
          target
          Value.print
          impl)));
  C.state.interpreter.cast_impls.map
  <- Types.ValueMap.union
       (fun target impls_a impls_b ->
          Some
            (Types.ValueMap.union
               (fun value a b ->
                  error
                    span
                    "conflicting impls of cast %a as %a (%a and %a)"
                    Value.print
                    value
                    Value.print
                    target
                    Value.print
                    a
                    Value.print
                    b;
                  Some a)
               impls_a
               impls_b))
       C.state.interpreter.cast_impls.map
       result.cast_impls.map;
  C.state.interpreter.cast_impls.as_module
  <- Types.ValueMap.union
       (fun value a _b ->
          error span "conflicting impls of cast %a as module" Value.print value;
          Some a)
       C.state.interpreter.cast_impls.as_module
       result.cast_impls.as_module;
  result.value
;;

let inject_binding ~(only_compiler : bool) (binding : binding) (state : State.t) : unit =
  state.scope <- state.scope |> State.Scope.inject_binding binding;
  if not only_compiler
  then state.interpreter.scope |> Kast_interpreter.Scope.inject_binding binding
;;

let rec inject_pattern_bindings
          ~(only_compiler : bool)
          (pattern : pattern)
          (state : State.t)
  : unit
  =
  match pattern.shape with
  | P_Placeholder -> ()
  | P_Unit -> ()
  | P_Ref inner -> state |> inject_pattern_bindings ~only_compiler inner
  | P_Binding { bind_mode = _; binding } -> state |> inject_binding ~only_compiler binding
  | P_Tuple { parts } ->
    parts
    |> List.iter (fun (part : _ Types.tuple_part_of) ->
      match part with
      | Field { label = _; label_span = _; field = field_pattern } ->
        state |> inject_pattern_bindings ~only_compiler field_pattern
      | Unpack pattern_to_unpack ->
        state |> inject_pattern_bindings ~only_compiler pattern_to_unpack)
  | P_Variant { label = _; label_span = _; value } ->
    (match value with
     | None -> ()
     | Some value -> inject_pattern_bindings ~only_compiler value state)
  | P_Error -> ()
;;

let rec inject_assignee_bindings
          ~(only_compiler : bool)
          (assignee : Expr.assignee)
          (state : State.t)
  : unit
  =
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit -> ()
  | A_Place _ -> ()
  | A_Tuple { parts } ->
    parts
    |> List.iter (fun (part : _ Types.tuple_part_of) ->
      match part with
      | Field { label = _; label_span = _; field = assignee_field } ->
        state |> inject_assignee_bindings ~only_compiler assignee_field
      | Unpack assignee_to_unpack ->
        state |> inject_assignee_bindings ~only_compiler assignee_to_unpack)
  | A_Let pattern -> state |> inject_pattern_bindings ~only_compiler pattern
  | A_Error -> ()
;;

let finish_compiling (def : Types.maybe_compiled_fn) (compiled : Types.compiled_fn) =
  def.compiled <- Some compiled;
  let fs = def.on_compiled in
  def.on_compiled <- [];
  fs |> List.iter (fun f -> f ())
;;
