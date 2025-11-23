open Std
open Kast_util
open Compiler_types
open Kast_types
open Error
module Ast = Kast_ast
module Inference = Kast_inference

module CompiledKind = struct
  type 'a t = 'a compiled_kind

  let print : 'a. formatter -> 'a compiled_kind -> unit =
   fun (type a) fmt (kind : a compiled_kind) : unit ->
    match kind with
    | Assignee -> fprintf fmt "assignee expr"
    | Expr -> fprintf fmt "expr"
    | TyExpr -> fprintf fmt "type expr"
    | Pattern -> fprintf fmt "pattern"
end

module type S = sig
  val state : State.t
  val compile : 'a. ?state:State.t -> 'a compiled_kind -> Ast.t -> 'a
end

type compiler = { compile : 'a. 'a compiled_kind -> Ast.t -> 'a }

let get_data : 'a. 'a compiled_kind -> 'a -> ir_data =
 fun (type a) (kind : a compiled_kind) (compiled : a) : ir_data ->
  match kind with
  | Expr -> compiled.data
  | Assignee -> compiled.data
  | TyExpr -> compiled.data
  | Pattern -> compiled.data

let update_data : 'a. 'a compiled_kind -> 'a -> (ir_data -> ir_data) -> 'a =
 fun (type a) (kind : a compiled_kind) (compiled : a) (f : ir_data -> ir_data) :
     a ->
  match kind with
  | Expr -> { compiled with data = f compiled.data }
  | Assignee -> { compiled with data = f compiled.data }
  | TyExpr -> { compiled with data = f compiled.data }
  | Pattern -> { compiled with data = f compiled.data }

let eval_ty (module C : S) (ast : Ast.t) : ty * Expr.ty =
  let ty_expr = C.compile TyExpr ast in
  let ty : ty =
    ty_expr.data.ty
    |> Inference.Ty.expect_inferred_as ~span:ty_expr.data.span
         (Ty.inferred ~span:ty_expr.data.span T_Ty);
    Kast_interpreter.eval_ty C.state.interpreter ty_expr
  in
  (ty, ty_expr)

let eval ~(ty : ty) (module C : S) (ast : Ast.t) : value * expr =
  let expr = C.compile Expr ast in
  let value : value =
    expr.data.ty |> Inference.Ty.expect_inferred_as ~span:expr.data.span ty;
    Kast_interpreter.eval C.state.interpreter expr
  in
  (value, expr)

let import ~(span : span) (module C : S) (uri : Uri.t) : value =
  let import_cache = C.state.import_cache in
  let result : State.imported =
    match UriMap.find_opt uri import_cache.by_uri with
    | None ->
        Log.trace (fun log -> log "Importing %a" Uri.print uri);
        Effect.perform (CompilerEffect.FileStartedProcessing uri);
        import_cache.by_uri <-
          UriMap.add uri (InProgress : State.import) import_cache.by_uri;
        let state : State.t =
          {
            currently_compiled_file = Some uri;
            custom_syntax_impls = Hashtbl.create 0;
            scope = C.state.scope;
            import_cache;
            interpreter = C.state.interpreter;
          }
        in
        let source = Source.read uri in
        let parsed = Kast_parser.parse source Kast_default_syntax.ruleset in
        let expr =
          match parsed.ast with
          | Some ast -> C.compile ~state Expr ast
          | None ->
              E_Constant { shape = V_Unit }
              |> Init.init_expr (Span.beginning_of source.uri) state
        in
        let value : value = Kast_interpreter.eval state.interpreter expr in
        Effect.perform
          (CompilerEffect.FileImported { uri; parsed; compiled = expr; value });
        let imported : State.imported =
          { value; custom_syntax_impls = state.custom_syntax_impls }
        in
        import_cache.by_uri <-
          UriMap.add uri (Imported imported : State.import) import_cache.by_uri;
        Log.trace (fun log -> log "Imported %a" Uri.print uri);
        imported
    | Some (Imported value) -> value
    | Some InProgress ->
        error span "No recursive imports!";
        { value = { shape = V_Error }; custom_syntax_impls = Hashtbl.create 0 }
  in
  Hashtbl.add_seq C.state.custom_syntax_impls
    (Hashtbl.to_seq result.custom_syntax_impls);
  result.value

let inject_binding (binding : binding) (state : State.t) : unit =
  state.scope <- state.scope |> State.Scope.inject_binding binding;
  state.interpreter.scope |> Kast_interpreter.Scope.inject_binding binding

let rec inject_pattern_bindings (pattern : pattern) (state : State.t) : unit =
  match pattern.shape with
  | P_Placeholder -> ()
  | P_Unit -> ()
  | P_Binding binding -> state |> inject_binding binding
  | P_Tuple { tuple } ->
      tuple |> Tuple.to_seq
      |> Seq.iter
           (fun
             ( _member,
               ({ label_span = _; label = _; field = field_pattern } :
                 pattern Types.tuple_field_of) )
           -> state |> inject_pattern_bindings field_pattern)
  | P_Variant { label = _; label_span = _; value } -> (
      match value with
      | None -> ()
      | Some value -> inject_pattern_bindings value state)
  | P_Error -> ()

let inject_assignee_bindings (assignee : Expr.assignee) (state : State.t) : unit
    =
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit -> ()
  | A_Binding _ -> ()
  | A_Let pattern -> state |> inject_pattern_bindings pattern
  | A_Error -> ()
