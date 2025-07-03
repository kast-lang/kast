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

let import ~(span : span) (module C : S) (path : path) : value =
  let path : path =
    match path with
    | File path -> (
        match path |> String.strip_prefix ~prefix:"./" with
        | Some relative -> (
            match span.filename with
            | File current ->
                File (Filename.concat (Filename.dirname current) relative)
            | _ -> error span "imports only work from regular files")
        | None -> error span "only relative paths are supported for now")
    | _ -> path
  in
  let source = Source.read path in
  let imported = C.state.imported in
  match PathMap.find_opt path imported.by_path with
  | None ->
      let state = State.blank ~imported in
      (* TODO *)
      let state = C.state in
      imported.by_path <-
        PathMap.add path (InProgress : State.import) imported.by_path;
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
      imported.by_path <-
        PathMap.add path (Imported value : State.import) imported.by_path;
      value
  | Some (Imported value) -> value
  | Some InProgress -> error span "No recursive imports!"
