open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast

type state = State.t

let init : unit -> state = fun () -> { interpreter = Interpreter.init () }

let rec compile : state -> Ast.t -> expr =
 fun state ast ->
  match ast.shape with
  | Ast.Simple { token; _ } -> (
      match token.shape with
      | Token.Shape.Ident ident -> { shape = E_Binding { name = ident.name } }
      | Token.Shape.String s ->
          { shape = E_Constant { shape = V_String s.contents } }
      | Token.Shape.Number { raw; _ } ->
          let value = Int32.of_string raw in
          { shape = E_Constant { shape = V_Int32 value } }
      | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
          unreachable "!")
  | Ast.Complex { rule; parts = _; children } -> (
      match rule.name |> String.strip_prefix ~prefix:"core:" with
      | Some name ->
          let handler = Core_syntax.handlers |> StringMap.find name in
          handler ~compile ~state children
      | None -> fail "todo compile syntax rule %S" rule.name)
  | Ast.Syntax _ -> fail "todo"
