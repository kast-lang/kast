open Std
open Util

let ruleset : Parser.ruleset = Parser.RuleSet.parse_lines [%blob "rules"]

type ast =
  | Simple of string
  | Complex of { name : string; children : ast tuple }

let rec print fmt = function
  | Simple s -> fprintf fmt "%S" s
  | Complex { name; children } ->
      fprintf fmt "%S %a" name (Tuple.print print) children

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
          name = Tuple.get_named "name" children |> get_name;
          children = Tuple.get_named "children" children |> collect_children;
        }
  | _ -> unreachable "process %a" Ast.print ast

and collect_children ast : ast tuple =
  match ast.kind with
  | Complex { name = "trailing comma"; children } ->
      Tuple.get_unnamed 0 children |> collect_children
  | Complex { name = "comma"; children } ->
      if
        Array.length children.unnamed <> 2
        || not (StringMap.is_empty children.named)
      then Parser.error "comma is incorrect structure: %a" Ast.print ast;
      let a = Tuple.get_unnamed 0 children |> collect_children in
      let b = Tuple.get_unnamed 1 children |> collect_children in
      Tuple.merge a b
  | Complex { name = "named"; children } ->
      let name = Tuple.get_named "name" children |> get_name in
      let value = Tuple.get_named "value" children |> process in
      Tuple.make [] [ (name, value) ]
  | _ -> Tuple.make [ process ast ] []

let parse : source -> ast option =
 fun source ->
  let ast = Parser.parse source ruleset in
  Log.debug "@[<v>Parsed: %a@]" (Option.print Ast.print) ast;
  ast |> Option.map process
