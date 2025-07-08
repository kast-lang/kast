open Std
open Kast_util
open Kast_types
open Error
module Ast = Kast_ast
module Inference = Kast_inference

type _ compiled_kind =
  | Assignee : Expr.assignee compiled_kind
  | Expr : expr compiled_kind
  | TyExpr : Expr.ty compiled_kind
  | Pattern : pattern compiled_kind

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
         (Ty.inferred T_Ty);
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
  let source = Source.read uri in
  let imported = C.state.imported in
  match UriMap.find_opt uri imported.by_uri with
  | None ->
      let state = State.blank ~imported in
      (* TODO *)
      let state = C.state in
      imported.by_uri <-
        UriMap.add uri (InProgress : State.import) imported.by_uri;
      let ({ ast; _ } : Kast_parser.result) =
        Kast_parser.parse source Kast_default_syntax.ruleset
      in
      let value : value =
        match ast with
        | Some ast ->
            let expr = C.compile ~state Expr ast in
            Kast_interpreter.eval state.interpreter expr
        | None -> { shape = V_Unit }
      in
      imported.by_uri <-
        UriMap.add uri (Imported value : State.import) imported.by_uri;
      value
  | Some (Imported value) -> value
  | Some InProgress ->
      error span "No recursive imports!";
      { shape = V_Error }

let inject_binding (binding : binding) (state : State.t) : unit =
  state.scope <- state.scope |> State.Scope.inject_binding binding

let rec inject_pattern_bindings (pattern : pattern) (state : State.t) : unit =
  match pattern.shape with
  | P_Placeholder -> ()
  | P_Unit -> ()
  | P_Binding binding -> state |> inject_binding binding
  | P_Tuple { tuple } ->
      tuple |> Tuple.to_seq
      |> Seq.iter (fun (_member, field_pattern) ->
             state |> inject_pattern_bindings field_pattern)
  | P_Error -> ()

let inject_assignee_bindings (assignee : Expr.assignee) (state : State.t) : unit
    =
  match assignee.shape with
  | A_Placeholder -> ()
  | A_Unit -> ()
  | A_Binding _ -> ()
  | A_Let pattern -> state |> inject_pattern_bindings pattern
  | A_Error -> ()

module Effect = struct
  type 'a file_included = {
    uri : Uri.t;
    parsed : Kast_parser.result;
    kind : 'a compiled_kind;
    compiled : 'a;
  }

  type _ Effect.t += FileIncluded : 'a. 'a file_included -> unit Effect.t
  type _ Effect.t += FindStd : Uri.t Effect.t
end
