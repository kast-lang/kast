open Std
open Kast_util
module Token = Kast_token
module Parser = Kast_parser
module Ast = Kast_ast
module Lexer = Kast_lexer
module Syntax = Kast_syntax

type parent = {
  wrapped : bool;
  rule : Syntax.rule;
}

let format : formatter -> Parser.result -> unit =
 fun fmt { ast; trailing_comments; eof = _ } ->
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

  let print_ast : formatter -> Ast.t -> unit =
   fun fmt ast ->
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
      | Error _ -> fail "Can't format code with errors"
      | Simple { comments_before; token } ->
          comments_before
          |> List.iter (fun (comment : Token.comment) ->
                 preserve_newline_after_comment comment.span;
                 prev_comment_span := Some comment.span;
                 fprintf fmt "%s" comment.shape.raw);
          preserve_newline_after_comment token.span;
          fprintf fmt "%s" (Token.raw token |> Option.get)
      | Complex { rule; root; _ } ->
          let wrapped =
            match parent with
            | Some { wrapped; rule = parent_rule } ->
                wrapped && rule.priority = parent_rule.priority
            | None -> false
          in
          let wrapped =
            wrapped || ast.span.start.line <> ast.span.finish.line
          in
          print_group rule root.rule wrapped root
      | Syntax { tokens; value_after; _ } -> (
          let pos = ref (List.head tokens).span.start in
          tokens
          |> List.iter (fun (token : Token.t) ->
                 if token.span.start > !pos then fprintf fmt " ";
                 fprintf fmt "%s" (Token.raw token |> Option.get);
                 pos := token.span.finish);
          match value_after with
          | None -> ()
          | Some value ->
              print_newline fmt ();
              print_ast ~parent:None value)
    and print_group rule (group_rule : Syntax.Rule.group option) wrapped
        ({ parts; _ } : Ast.group) =
      print_parts rule wrapped
        (match group_rule with
        | None -> rule.parts
        | Some group -> group.parts)
        parts
    and print_parts rule wrapped (rule_parts : Syntax.Rule.part list)
        (parts : Ast.part list) =
      match (rule_parts, parts) with
      | _, Comment comment :: rest_parts ->
          preserve_newline_after_comment comment.span;
          fprintf fmt "%s" comment.shape.raw;
          prev_comment_span := Some comment.span;
          print_parts rule wrapped rule_parts rest_parts
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
      | Group rule_group :: rest_rule_parts, Group group :: rest_parts
        when Some rule_group = group.rule ->
          print_group rule group.rule wrapped group;
          print_parts rule wrapped rest_rule_parts rest_parts
      | Group _ :: rest_rule_parts, _ ->
          (* Group was skipped *)
          print_parts rule wrapped rest_rule_parts parts
      | _ ->
          unreachable "print_parts %a %a"
            (Option.print Syntax.Rule.Part.print)
            (List.head_opt rule_parts)
            (Option.print Ast.Part.print)
            (List.head_opt parts)
    in

    print_ast ~parent:None ast
  in
  (match ast with
  | Some ast -> print_ast fmt ast
  | None -> ());
  trailing_comments
  |> List.iter (fun (comment : Token.comment) ->
         preserve_newline_after_comment comment.span;
         prev_comment_span := Some comment.span;
         fprintf fmt "%s" comment.shape.raw);
  fprintf fmt "\n"
