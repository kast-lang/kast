open Std
open Util

let ruleset : Parser.ruleset = Parser.RuleSet.parse_lines [%blob "rules"]

type ast = Simple of string | Complex of { name : string; children : ast row }

let rec print fmt = function
  | Simple s -> fprintf fmt "%S" s
  | Complex { name; children } ->
      fprintf fmt "%S %a" name (Row.print print) children

let get_name : Ast.t -> string = function
  | { kind = Simple { token = Ident { raw; _ } }; _ } -> raw
  | other -> unreachable "get_name %a" Ast.print other

let rec process : Ast.t -> ast =
 fun ast ->
  match ast.kind with
  | Simple { token } -> Simple (Lexer.Token.raw token |> Option.get)
  | Complex { name = "complex"; children } ->
      Complex
        {
          name = Row.get_named "name" children |> get_name;
          children = Row.get_named "children" children |> collect_children;
        }
  | _ -> unreachable "process %a" Ast.print ast

and collect_children ast : ast row =
  match ast.kind with
  | Complex { name = "trailing comma"; children } ->
      Row.get_unnamed 0 children |> collect_children
  | Complex { name = "comma"; children } ->
      if
        Array.length children.unnamed <> 2
        || not (StringMap.is_empty children.named)
      then Parser.error "comma is incorrect structure: %a" Ast.print ast;
      let a = Row.get_unnamed 0 children |> collect_children in
      let b = Row.get_unnamed 1 children |> collect_children in
      Row.merge a b
  | Complex { name = "named"; children } ->
      let name = Row.get_named "name" children |> get_name in
      let value = Row.get_named "value" children |> process in
      Row.make [] [ (name, value) ]
  | _ -> Row.make [ process ast ] []

let parse : source -> ast option =
 fun source ->
  let ast = Parser.parse source ruleset in
  Log.trace "@[<v>Parsed: %a@]" (Option.print Ast.print) ast;
  ast |> Option.map process
