open Std
open Kast_util
module Token = Kast_token
module Parser = Kast_parser
module Ast = Kast_ast.T
module Lexer = Kast_lexer
module Syntax = Kast_syntax

type parent =
  { wrapped : bool
  ; rule : Syntax.rule
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
  let print_newline_if_not_yet () = if not !printed_newline then print_newline () in
  let printf format =
    printed_newline := false;
    fprintf fmt format
  in
  let prev_span : span ref = ref <| Span.beginning_of Uri.empty in
  let prev_was_comment : Token.Shape.comment_ty option ref = ref None in
  let print ?(is_comment : Token.Shape.comment_ty option) (span : span) f value =
    (* We create spans with Uri.empty in rewriter and we want to skip them *)
    let span = if Uri.equal span.uri Uri.empty then !prev_span else span in
    if span.start.line > !prev_span.finish.line + 1 then print_newline ();
    (match !prev_was_comment with
     | None ->
       (match is_comment with
        | None -> ()
        | Some _ ->
          if
            !prev_span.finish.line = span.start.line
            && !prev_span.finish <> Position.beginning
          then printf " "
          else print_newline_if_not_yet ())
     | Some Line -> print_newline_if_not_yet ()
     | Some Block ->
       if
         !prev_span.finish.line = span.start.line
         && !prev_span.finish <> Position.beginning
       then printf " "
       else print_newline_if_not_yet ());
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
    let rec print_ast ~filter ~parent (ast : Ast.t) =
      match ast.shape with
      | Error _ -> fail "Can't format code with errors"
      | Empty -> ()
      | Simple { comments_before; token } ->
        comments_before
        |> List.iter (fun (comment : Token.comment) ->
          print ~is_comment:comment.shape.ty comment.span String.print comment.shape.raw);
        print token.span String.print (Token.raw token |> Option.get)
      | Complex { rule; root } ->
        let wrapped =
          match parent with
          | Some { wrapped; rule = parent_rule } ->
            wrapped
            && rule.wrap_mode = IfAnyAssociative
            && rule.priority = parent_rule.priority
          | None -> false
        in
        let wrapped = wrapped || ast.data.start.line <> ast.data.finish.line in
        (* detect duplicate parens *)
        if String.equal rule.name "core:scope" && filter = Syntax.Rule.Priority.Any
        then (
          let child =
            root.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
          in
          print_ast ~filter:Any ~parent:(Some { wrapped; rule }) child)
        else print_group rule root.rule wrapped rule.wrap_mode root
      | Syntax { tokens; value_after; _ } ->
        let pos = ref (List.head tokens).span.start in
        tokens
        |> List.iter (fun (token : Token.t) ->
          if token.span.start > !pos then printf " ";
          print token.span String.print (Token.raw token |> Option.get);
          pos := token.span.finish);
        (match value_after with
         | None -> ()
         | Some value ->
           print_newline ();
           print_ast ~filter ~parent:None value)
    and print_group
          rule
          (group_rule : Syntax.Rule.group option)
          (wrapped : bool)
          (wrap_mode : Syntax.Rule.wrap_mode)
          ({ parts; span; _ } : Ast.group)
      =
      let wrapped, wrap_mode =
        match group_rule with
        | Some { wrap_mode = Some override_wrap_mode; _ } ->
          let group_wrapped = span.start.line <> span.finish.line in
          group_wrapped, override_wrap_mode
        | _ -> wrapped, wrap_mode
      in
      print_parts
        rule
        wrapped
        wrap_mode
        (match group_rule with
         | None -> rule.parts
         | Some group -> group.parts)
        parts
    and print_parts
          rule
          (wrapped : bool)
          (wrap_mode : Syntax.Rule.wrap_mode)
          (rule_parts : Syntax.Rule.part list)
          (parts : Ast.part list)
      =
      match rule_parts, parts with
      | _, Comment comment :: rest_parts ->
        print ~is_comment:comment.shape.ty comment.span String.print comment.shape.raw;
        print_parts rule wrapped wrap_mode rule_parts rest_parts
      | [], [] -> ()
      | Whitespace { nowrap; wrap } :: rest_rule_parts, _ ->
        (match wrap_mode with
         | Never -> print_whitespace nowrap
         | Always -> print_whitespace wrap
         | IfAnyAssociative | IfAnyNonAssociative ->
           if wrapped then print_whitespace wrap else print_whitespace nowrap);
        print_parts rule wrapped wrap_mode rest_rule_parts parts
      | Keyword keyword :: rest_rule_parts, Keyword keyword_token :: rest_parts ->
        print keyword_token.span String.print keyword;
        print_parts rule wrapped wrap_mode rest_rule_parts rest_parts
      | Value binding :: rest_rule_parts, Value value :: rest_parts ->
        print_ast ~filter:binding.priority ~parent:(Some { rule; wrapped }) value;
        print_parts rule wrapped wrap_mode rest_rule_parts rest_parts
      | Group rule_group :: rest_rule_parts, Group group :: rest_parts
        when Some rule_group = group.rule ->
        print_group rule group.rule wrapped wrap_mode group;
        print_parts rule wrapped wrap_mode rest_rule_parts rest_parts
      | Group _ :: rest_rule_parts, _ ->
        (* Group was skipped *)
        print_parts rule wrapped wrap_mode rest_rule_parts parts
      | _ ->
        unreachable
          "print_parts %a %a"
          (Option.print Syntax.Rule.Part.print)
          (List.head_opt rule_parts)
          (Option.print Ast.Part.print)
          (List.head_opt parts)
    in
    print_ast ~filter:Any ~parent:None ast
  in
  print_ast ast;
  trailing_comments
  |> List.iter (fun (comment : Token.comment) ->
    print ~is_comment:comment.shape.ty comment.span String.print comment.shape.raw);
  print_newline ()
;;
