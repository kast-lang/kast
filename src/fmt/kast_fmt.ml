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
      fprintf fmt "    "
    done
  in

  let current_indent = ref 0 in

  let printed_newline = ref true in

  let print_newline () =
    fprintf fmt "\n";
    print_indent !current_indent;
    printed_newline := true
  in

  let print_newline_if_not_yet () =
    if not !printed_newline then print_newline ()
  in

  let printf format =
    printed_newline := false;
    fprintf fmt format
  in

  let prev_span : span ref = ref <| Span.beginning_of Uri.empty in
  let prev_was_comment : bool ref = ref false in

  let print ?(is_comment = false) (span : span) f value =
    if span.start.line > !prev_span.finish.line + 1 then print_newline ();
    if is_comment || !prev_was_comment then
      if
        !prev_span.finish.line = span.start.line
        && !prev_span.finish <> Position.beginning
      then printf " "
      else print_newline_if_not_yet ();
    f fmt value;
    printed_newline := false;
    prev_span := span;
    prev_was_comment := is_comment
  in

  let print_ast : Ast.t -> unit =
   fun ast ->
    let print_whitespace (s : string) =
      s
      |> String.iter (function
        | '\n' -> print_newline ()
        | '\t' ->
            print_indent 1;
            current_indent := !current_indent + 1
        | '\\' -> current_indent := !current_indent - 1
        | c -> printf "%c" c)
    in
    let rec print_ast ~parent (ast : Ast.t) =
      match ast.shape with
      | Error _ -> fail "Can't format code with errors"
      | Simple { comments_before; token } ->
          comments_before
          |> List.iter (fun (comment : Token.comment) ->
              print ~is_comment:true comment.span String.print comment.shape.raw);
          print token.span String.print (Token.raw token |> Option.get)
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
              if token.span.start > !pos then printf " ";
              print token.span String.print (Token.raw token |> Option.get);
              pos := token.span.finish);
          match value_after with
          | None -> ()
          | Some value ->
              print_newline ();
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
          print ~is_comment:true comment.span String.print comment.shape.raw;
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
          print keyword_token.span String.print keyword;
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
  | Some ast -> print_ast ast
  | None -> ());
  trailing_comments
  |> List.iter (fun (comment : Token.comment) ->
      print ~is_comment:true comment.span String.print comment.shape.raw);
  print_newline ()
