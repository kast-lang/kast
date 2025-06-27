open Std
open Kast

type args = { path : string option }

let parse : string list -> args = function
  | [] -> { path = None }
  | [ path ] -> { path = Some path }
  | first :: _rest -> fail "unexpected arg %S" first

let read path : source =
  let path =
    match path with
    | None | Some "-" -> None
    | Some path -> Some path
  in
  let channel =
    match path with
    | Some path -> In_channel.open_text path
    | None -> In_channel.stdin
  in
  let contents = In_channel.input_all channel in
  let filename =
    match path with
    | Some path -> path
    | None -> "<stdin>"
  in
  { contents; filename }

let print_ast : Parser.ruleset -> formatter -> Ast.t -> unit =
 fun ruleset fmt ast ->
  let rec print_ast (ast : Ast.t) =
    match ast.shape with
    | Simple { comments_before; token } ->
        fprintf fmt "%s" (Lexer.Token.raw token.value |> Option.get)
    | Complex { name; parts; _ } ->
        let rule = Parser.RuleSet.find_rule name ruleset in
        print_parts rule.parts parts
  and print_parts (rule_parts : Parser.Rule.part list) (parts : Ast.part list) =
    match (rule_parts, parts) with
    | [], [] -> ()
    | Whitespace { nowrap; wrap } :: rest_rule_parts, _ ->
        fprintf fmt "%s" nowrap;
        print_parts rest_rule_parts parts
    | Keyword keyword :: rest_rule_parts, Keyword _ :: rest_parts ->
        fprintf fmt "%s" keyword;
        print_parts rest_rule_parts rest_parts
    | Value _ :: rest_rule_parts, Value value :: rest_parts ->
        print_ast value;
        print_parts rest_rule_parts rest_parts
    | _ -> unreachable "todo"
  in

  print_ast ast

let format : formatter -> Parser.result -> unit =
 fun fmt { ast; trailing_comments } ->
  (* TODO not necessarily default *)
  let ruleset = Default_syntax.ruleset in
  match ast with
  | Some ast -> fprintf fmt "@[<v>%a@]" (print_ast ruleset) ast
  | None -> ()

let run : args -> unit =
 fun { path } ->
  let source = read path in
  let ruleset = Default_syntax.ruleset in
  Parser.parse source ruleset |> format Format.std_formatter;
  println ""
