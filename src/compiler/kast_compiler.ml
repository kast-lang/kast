open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Init
include Error

type state = State.t

(* TODO compile_for - figure out *)
let init : compile_for:Interpreter.state -> state =
 fun ~compile_for ->
  let scope = State.Scope.init () in
  let scope =
    StringMap.fold
      (fun name value scope ->
        scope
        |> State.Scope.inject_binding
             {
               name;
               ty = Value.ty_of value;
               span = Span.fake "<interpreter>";
               references = [];
             })
      compile_for.scope.bindings scope
  in
  { scope; interpreter = compile_for }

type 'a compiled_kind = 'a Compiler.compiled_kind

let get_data = Compiler.get_data

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
 fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; span } : Ast.t = ast in
  match ast.shape with
  | Ast.Simple { token; _ } -> (
      match kind with
      | Expr -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              E_Binding
                (State.Scope.find_binding ~from:ast.span ident state.scope)
              |> init_expr span
          | Token.Shape.String s ->
              E_Constant { shape = V_String s.contents } |> init_expr span
          | Token.Shape.Number { raw; _ } ->
              let value = Int32.of_string raw in
              E_Constant { shape = V_Int32 value } |> init_expr span
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!")
      | TyExpr -> TE_Expr (compile state Expr ast) |> init_ty_expr span
      | Assignee -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              A_Binding
                (State.Scope.find_binding ~from:ast.span ident state.scope)
              |> init_assignee span
          | Token.Shape.String _ -> error span "string can't be assignee"
          | Token.Shape.Number _ -> error span "number can't be assignee"
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!")
      | Pattern -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              let binding : binding =
                {
                  name = ident.name;
                  ty = Ty.new_not_inferred ();
                  span = ast.span;
                  references = [];
                }
              in
              state.scope <- state.scope |> State.Scope.inject_binding binding;
              P_Binding binding |> init_pattern span
          | Token.Shape.String _ -> error span "string can't be pattern"
          | Token.Shape.Number _ -> error span "number can't be pattern"
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!"))
  | Ast.Complex { rule; root } -> (
      match rule.name |> String.strip_prefix ~prefix:"core:" with
      | Some name -> (
          let handler =
            Core_syntax.handlers |> StringMap.find_opt name
            |> Option.unwrap_or_else (fun () ->
                   error span "there is no core syntax %S" name)
          in
          try handler.handle (make_compiler state) kind root ast.span
          with exc ->
            Log.error "While processing core syntax %S at %a" name Span.print
              ast.span;
            raise exc)
      | None -> error span "todo compile syntax rule %S" rule.name)
  | Ast.Syntax _ -> error span "todo %s" __LOC__

and make_compiler (original_state : state) : (module Compiler.S) =
  (module struct
    let state = original_state

    let compile (type b) ?state (kind : b compiled_kind) (ast : Ast.t) : b =
      compile (state |> Option.value ~default:original_state) kind ast
  end : Compiler.S)
