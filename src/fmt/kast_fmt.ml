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

type parent = {
  wrapped : bool;
  rule : Parser.rule;
}

let format : formatter -> Parser.result -> unit =
 fun fmt { ast; trailing_comments } ->
  let print_indent level =
    for _ = 1 to level do
      fprintf fmt "  "
    done
  in

  let current_indent = ref 0 in

  let print_newline fmt () =
    fprintf fmt "\n";
    print_indent !current_indent
  in

  let prev_comment_span : span option ref = ref None in

  let preserve_newline_after_comment (after : span) =
    match !prev_comment_span with
    | Some before ->
        if before.finish.line <> after.start.line then print_newline fmt ()
        else fprintf fmt " ";
        prev_comment_span := None
    | None -> ()
  in

  let print_ast : Parser.ruleset -> formatter -> Ast.t -> unit =
   fun ruleset fmt ast ->
    let print_whitespace (s : string) =
      s
      |> String.iter (function
           | '\n' -> print_newline fmt ()
           | '\t' ->
               print_indent 1;
               current_indent := !current_indent + 1
           | '\\' -> current_indent := !current_indent - 1
           | c -> fprintf fmt "%c" c)
    in
    let rec print_ast ~parent (ast : Ast.t) =
      match ast.shape with
      | Simple { comments_before; token } ->
          comments_before
          |> List.iter (fun (comment : Lexer.Token.comment spanned) ->
                 preserve_newline_after_comment comment.span;
                 prev_comment_span := Some comment.span;
                 fprintf fmt "%s" comment.value.raw);
          preserve_newline_after_comment token.span;
          fprintf fmt "%s" (Lexer.Token.raw token.value |> Option.get)
      | Complex { name; parts; _ } ->
          let rule = Parser.RuleSet.find_rule name ruleset in
          let wrapped =
            match parent with
            | Some { wrapped; rule = parent_rule } ->
                wrapped && rule.priority = parent_rule.priority
            | None -> false
          in
          let wrapped =
            wrapped || ast.span.start.line <> ast.span.finish.line
          in
          print_parts rule wrapped rule.parts parts
    and print_parts rule wrapped (rule_parts : Parser.Rule.part list)
        (parts : Ast.part list) =
      match (rule_parts, parts) with
      | [], [] -> ()
      | Whitespace { nowrap; wrap } :: rest_rule_parts, _ ->
          (match rule.wrap_mode with
          | Never -> print_whitespace nowrap
          | Always -> print_whitespace wrap
          | IfAny ->
              if wrapped then print_whitespace wrap else print_whitespace nowrap);
          print_parts rule wrapped rest_rule_parts parts
      | Keyword keyword :: rest_rule_parts, Keyword keyword_token :: rest_parts
        ->
          preserve_newline_after_comment keyword_token.span;
          fprintf fmt "%s" keyword;
          print_parts rule wrapped rest_rule_parts rest_parts
      | Value _ :: rest_rule_parts, Value value :: rest_parts ->
          print_ast ~parent:(Some { rule; wrapped }) value;
          print_parts rule wrapped rest_rule_parts rest_parts
      | _, Comment comment :: rest_parts ->
          preserve_newline_after_comment comment.span;
          fprintf fmt "%s" comment.value.raw;
          prev_comment_span := Some comment.span;
          print_parts rule wrapped rule_parts rest_parts
      | _ -> unreachable "todo"
    in

    print_ast ~parent:None ast
  in
  (* TODO not necessarily default *)
  let ruleset = Default_syntax.ruleset in
  (match ast with
  | Some ast -> fprintf fmt "@[<v>%a@]" (print_ast ruleset) ast
  | None -> ());
  trailing_comments
  |> List.iter (fun (comment : Lexer.Token.comment spanned) ->
         preserve_newline_after_comment comment.span;
         prev_comment_span := Some comment.span;
         fprintf fmt "%s" comment.value.raw)

let run : args -> unit =
 fun { path } ->
  let source = read path in
  let ruleset = Default_syntax.ruleset in
  let parsed = Parser.parse source ruleset in
  parsed |> format Format.std_formatter;
  println ""
