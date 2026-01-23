open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
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

module InitAst = struct
  type ctx = (int, Ast.t) Hashtbl.t
  type _ Effect.t += GetCtx : ctx Effect.t

  let rec init_ast : Kast_ast.T.t -> Ast.t =
    fun ast ->
    let id = Obj.magic ast in
    let ctx = Effect.perform GetCtx in
    match Hashtbl.find_opt ctx id with
    | Some result -> result
    | None ->
      let shape : Ast.shape =
        match ast.shape with
        | Empty -> Empty
        | Simple simple -> Simple simple
        | Complex { rule; root } -> Complex { rule; root = init_ast_group root }
        | Syntax { comments_before; mode; tokens; value_after } ->
          Syntax
            { comments_before
            ; mode
            ; tokens
            ; value_after = value_after |> Option.map init_ast
            }
        | Error { parts } -> Error { parts = parts |> List.map init_ast_part }
      in
      let data : Ast.Data.t = { span = ast.data; hygiene = DefSite; def_site = None } in
      let result : Ast.t = { shape; data } in
      Hashtbl.add ctx id result;
      result

  and init_ast_child : Kast_ast.T.child -> Ast.child =
    fun child ->
    match child with
    | Ast ast -> Ast (init_ast ast)
    | Group g -> Group (init_ast_group g)

  and init_ast_group : Kast_ast.T.group -> Ast.group =
    fun { rule; parts; children; span } ->
    let children = children |> Tuple.map init_ast_child in
    { rule; parts = parts |> List.map init_ast_part; children; span }

  and init_ast_part : Kast_ast.T.part -> Ast.part =
    fun part ->
    match part with
    | Comment c -> Comment c
    | Value ast -> Value (init_ast ast)
    | Keyword k -> Keyword k
    | Group g -> Group (init_ast_group g)
  ;;

  let init_ast ast =
    let ctx : ctx = Hashtbl.create 0 in
    try init_ast ast with
    | effect GetCtx, k -> Effect.continue k ctx
  ;;
end

let init_ast = InitAst.init_ast

module InitAstDefSite = struct
  type ctx =
    { inited : (int, Ast.t) Hashtbl.t
    ; scope : Types.compiler_scope
    }

  type _ Effect.t += GetCtx : ctx Effect.t

  let rec init_ast : Ast.t -> Ast.t =
    fun ast ->
    let id = Obj.magic ast in
    let ctx = Effect.perform GetCtx in
    match Hashtbl.find_opt ctx.inited id with
    | Some result -> result
    | None ->
      let shape : Ast.shape =
        match ast.shape with
        | Empty -> Empty
        | Simple simple -> Simple simple
        | Complex { rule; root } -> Complex { rule; root = init_ast_group root }
        | Syntax { comments_before; mode; tokens; value_after } ->
          Syntax
            { comments_before
            ; mode
            ; tokens
            ; value_after = value_after |> Option.map init_ast
            }
        | Error { parts } -> Error { parts = parts |> List.map init_ast_part }
      in
      let data : Ast.Data.t =
        { span = ast.data.span
        ; hygiene = ast.data.hygiene
        ; def_site = ast.data.def_site |> Option.or_ (Some ctx.scope)
        }
      in
      let result : Ast.t = { shape; data } in
      Hashtbl.add ctx.inited id result;
      result

  and init_ast_child : Ast.child -> Ast.child =
    fun child ->
    match child with
    | Ast ast -> Ast (init_ast ast)
    | Group g -> Group (init_ast_group g)

  and init_ast_group : Ast.group -> Ast.group =
    fun { rule; parts; children; span } ->
    let children = children |> Tuple.map init_ast_child in
    { rule; parts = parts |> List.map init_ast_part; children; span }

  and init_ast_part : Ast.part -> Ast.part =
    fun part ->
    match part with
    | Comment c -> Comment c
    | Value ast -> Value (init_ast ast)
    | Keyword k -> Keyword k
    | Group g -> Group (init_ast_group g)
  ;;

  let init_ast scope ast =
    let ctx : ctx = { inited = Hashtbl.create 0; scope } in
    try init_ast ast with
    | effect GetCtx, k -> Effect.continue k ctx
  ;;
end

let init_ast_def_site_delete = InitAstDefSite.init_ast

let update_data : 'a. 'a compiled_kind -> 'a -> (ir_data -> ir_data) -> 'a =
  fun (type a) (kind : a compiled_kind) (compiled : a) (f : ir_data -> ir_data) : a ->
  match kind with
  | Expr -> { compiled with data = f compiled.data }
  | Assignee -> { compiled with data = f compiled.data }
  | TyExpr -> { compiled with data = f compiled.data }
  | Pattern -> { compiled with data = f compiled.data }
  | PlaceExpr -> { compiled with data = f compiled.data }
;;

type 'a evaled_kind =
  | Expr : (expr * value) evaled_kind
  | TyExpr : (Expr.ty * ty) evaled_kind
  | Pattern : pattern evaled_kind

let data_append
      (type a)
      (added_kind : a evaled_kind)
      (added : a)
      (type b)
      (kind : b compiled_kind)
      (compiled : b)
  : unit
  =
  let evaled = (get_data kind compiled).evaled in
  match added_kind with
  | TyExpr -> evaled.ty_exprs <- added :: evaled.ty_exprs
  | Expr -> evaled.exprs <- added :: evaled.exprs
  | Pattern -> evaled.patterns <- added :: evaled.patterns
;;

let data_add
      (type a)
      (added_kind : a evaled_kind)
      (added : a)
      (type b)
      (kind : b compiled_kind)
      (compiled : b)
  : b
  =
  data_append added_kind added kind compiled;
  compiled
;;

let set_evaled (value : value) (type b) (kind : b compiled_kind) (compiled : b) : b =
  (get_data kind compiled).evaled.value <- Some value;
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
  PE_Temp expr |> Init.init_place_expr ast.data.span C.state
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
          scopes = C.state.scopes
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
      let expr = C.compile ~state Expr (parsed.ast |> init_ast) in
      let value : value = Kast_interpreter.eval state.interpreter expr in
      Effect.perform (CompilerEffect.FileImported { uri; parsed; compiled = expr; value });
      let imported : State.imported =
        { value
        ; custom_syntax_impls = state.custom_syntax_impls
        ; cast_impls = state.interpreter.cast_impls
        }
      in
      (let { value; custom_syntax_impls; cast_impls } : State.imported = imported in
       Kast_inference_completion.complete_value value;
       custom_syntax_impls
       |> Hashtbl.iter (fun _id impl -> Kast_inference_completion.complete_value impl);
       let { map = cast_impls; as_module = cast_as_module } : Types.cast_impls =
         cast_impls
       in
       cast_impls
       |> Types.ValueMap.iter (fun target impls ->
         Kast_inference_completion.complete_value target;
         impls
         |> Types.ValueMap.iter (fun value impl ->
           Kast_inference_completion.complete_value value;
           Kast_inference_completion.complete_value impl));
       cast_as_module
       |> Types.ValueMap.iter (fun value impl ->
         Kast_inference_completion.complete_value value;
         Kast_inference_completion.complete_value impl));
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

let add_local ~(only_compiler : bool) (local : State.Scope.local) (state : State.t) : unit
  =
  state.scopes <- state.scopes |> State.Scopes.add local;
  if not only_compiler
  then
    state.interpreter.scope
    |> Kast_interpreter.Scope.inject_binding (State.Scope.Local.binding local)
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
  | P_Binding { bind_mode = _; binding } ->
    state |> add_local ~only_compiler (Binding binding)
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

let rec local_place_expr span state (local : State.Scope.local) =
  match local with
  | Const _ -> PE_Temp (local_expr span state local) |> Init.init_place_expr span state
  | Binding binding -> PE_Binding binding |> Init.init_place_expr span state

and local_expr span state (local : State.Scope.local) =
  match local with
  | Const { place; binding } ->
    let value = Kast_interpreter.read_place place ~span in
    let expr = E_Constant { id = Id.gen (); value } |> Init.init_expr span state in
    expr.data.evaled.binding <- Some binding;
    expr
  | Binding _ -> E_Claim (local_place_expr span state local) |> Init.init_expr span state
;;
