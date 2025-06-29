open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Init

type state = State.t

let init : unit -> state = fun () -> { interpreter = Interpreter.init () }

type 'a compiled_kind = 'a Compiler.compiled_kind

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
 fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; span } : Ast.t = ast in
  match ast.shape with
  | Ast.Simple { token; _ } -> (
      match kind with
      | Expr -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              E_Binding { name = ident.name; ty = Ty.new_not_inferred () }
              |> init_expr span
          | Token.Shape.String s ->
              E_Constant { shape = V_String s.contents } |> init_expr span
          | Token.Shape.Number { raw; _ } ->
              let value = Int32.of_string raw in
              E_Constant { shape = V_Int32 value } |> init_expr span
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!")
      | Assignee -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              A_Binding { name = ident.name; ty = Ty.new_not_inferred () }
              |> init_assignee span
          | Token.Shape.String _ -> fail "string can't be assignee"
          | Token.Shape.Number _ -> fail "number can't be assignee"
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!")
      | Pattern -> (
          match token.shape with
          | Token.Shape.Ident ident ->
              P_Binding { name = ident.name; ty = Ty.new_not_inferred () }
              |> init_pattern span
          | Token.Shape.String _ -> fail "string can't be pattern"
          | Token.Shape.Number _ -> fail "number can't be pattern"
          | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
              unreachable "!"))
  | Ast.Complex { rule; parts = _; children } -> (
      match rule.name |> String.strip_prefix ~prefix:"core:" with
      | Some name ->
          let handler =
            Core_syntax.handlers |> StringMap.find_opt name
            |> Option.unwrap_or_else (fun () ->
                   fail "there is no core syntax %S" name)
          in
          handler.handle (make_compiler state) kind children ast.span
      | None -> fail "todo compile syntax rule %S" rule.name)
  | Ast.Syntax _ -> fail "todo %s" __LOC__

and make_compiler (state : state) : (module Compiler.S) =
  (module struct
    let compile (type b) (kind : b compiled_kind) (ast : Ast.t) : b =
      compile state kind ast
  end : Compiler.S)
