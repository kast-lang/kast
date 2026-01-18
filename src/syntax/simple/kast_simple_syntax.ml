open Std
open Kast_util
module Token = Kast_token
module Syntax = Kast_syntax
module Ast = Kast_ast.T
module Lexer = Kast_lexer
module Parser = Kast_parser

let ruleset : Parser.ruleset = Parser.Ruleset.parse_lines [%include_file "rules.ks"]

type ast =
  | Simple of string
  | Complex of
      { name : string
      ; children : ast tuple
      }

let rec print fmt = function
  | Simple s -> fprintf fmt "%S" s
  | Complex { name; children } -> fprintf fmt "%S %a" name (Tuple.print print) children
;;

let get_name : Ast.t -> string = function
  | { shape = Simple { token = { shape = Ident { raw; _ }; _ }; _ }; _ } -> raw
  | other -> unreachable "get_name %a" Ast.print other
;;

let rec process : Ast.t -> ast =
  fun ast ->
  match ast.shape with
  | Simple { token; _ } -> Simple (Token.raw token |> Option.get)
  | Complex { rule = { name = "complex"; _ }; root } ->
    let children = root.children |> Tuple.map Ast.Child.expect_ast in
    Complex
      { name = Tuple.get_named "name" children |> get_name
      ; children = Tuple.get_named "children" children |> collect_children
      }
  | _ -> unreachable "process %a" Ast.print ast

and collect_children ast : ast tuple =
  match ast.shape with
  | Complex { rule = { name = "trailing comma"; _ }; root } ->
    let children = root.children |> Tuple.map Ast.Child.expect_ast in
    Tuple.get_unnamed 0 children |> collect_children
  | Complex { rule = { name = "comma"; _ }; root } ->
    let children = root.children |> Tuple.map Ast.Child.expect_ast in
    if Array.length children.unnamed <> 2 || not (StringMap.is_empty children.named)
    then Parser.error ast.data "comma is incorrect structure: %a" Ast.print ast;
    let a = Tuple.get_unnamed 0 children |> collect_children in
    let b = Tuple.get_unnamed 1 children |> collect_children in
    Tuple.merge a b
  | Complex { rule = { name = "named"; _ }; root } ->
    let children = root.children |> Tuple.map Ast.Child.expect_ast in
    let name = Tuple.get_named "name" children |> get_name in
    let value = Tuple.get_named "value" children |> process in
    Tuple.make [] [ name, value ]
  | _ -> Tuple.make [ process ast ] []
;;

let parse : source -> ast =
  fun source ->
  let { ast; trailing_comments = _; eof = _ } : Parser.result =
    Parser.parse source ruleset
  in
  Log.trace (fun log -> log "Parsed: %a" Ast.print ast);
  process ast
;;
