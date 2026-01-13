open Std
open Kast_util
open Kast_highlight
module Parser = Kast_parser
module Syntax = Kast_syntax

module Args = struct
  type rewrite =
    { from : string
    ; into : string
    }

  type args =
    { paths : Uri.t list
    ; hl_output : Kast_highlight.output option
    ; rewrite : rewrite list
    ; inplace : bool
    }

  type t = args

  let rec parse : string list -> args = function
    | [] -> { paths = []; hl_output = Some Term; rewrite = []; inplace = false }
    | "--inplace" :: rest -> { (parse rest) with inplace = true }
    | "--rewrite" :: from :: into :: rest ->
      let rest = parse rest in
      { rest with rewrite = { from; into } :: rest.rewrite }
    | "--highlight" :: highlight :: rest ->
      let hl_output =
        match highlight with
        | "none" -> None
        | "html" -> Some Html
        | "term" | "terminal" -> Some Term
        | _ ->
          fail
            "Unexpected highlight output '%S', only 'html', 'term' or 'none' are allowed"
            highlight
      in
      { (parse rest) with hl_output }
    | first :: rest ->
      let args = parse rest in
      { args with paths = Uri.file first :: args.paths }
  ;;
end

let rewrite_one ({ from; into } : Args.rewrite) (ast : Ast.t) : Ast.t =
  let rule_into : Syntax.Rule.t =
    Kast_default_syntax.ruleset |> Parser.Ruleset.find_rule into
  in
  let rewritten_map = Hashtbl.create 0 in
  let rec rewrite_ast (ast : Ast.t) : Ast.t =
    match Hashtbl.find_opt rewritten_map ast with
    | Some result -> result
    | None ->
      Log.trace (fun log -> log "rewriting %a" Ast.print ast);
      let result =
        match ast.shape with
        | Ast.Empty -> ast
        | Ast.Simple _ -> ast
        | Ast.Complex { rule; root } ->
          if String.equal rule.name from
          then
            { shape =
                Ast.Complex
                  { rule = rule_into
                  ; root = rewrite_group_matched_rule root rule_into.parts None
                  }
            ; span = ast.span
            }
          else
            { shape = Ast.Complex { rule; root = rewrite_group root }; span = ast.span }
        | Ast.Syntax node ->
          { shape =
              Ast.Syntax
                { node with value_after = node.value_after |> Option.map rewrite_ast }
          ; span = ast.span
          }
        | Ast.Error _ -> ast
      in
      Hashtbl.add rewritten_map ast result;
      result
  and rewrite_group ({ rule; parts; children } : Ast.group) : Ast.group =
    { rule
    ; parts = parts |> List.map rewrite_part
    ; children = children |> Tuple.map rewrite_child
    }
  and rewrite_child (child : Ast.child) : Ast.child =
    match (child : Ast.child) with
    | Ast ast -> Ast (rewrite_ast ast)
    | Group group -> Group (rewrite_group group)
  and rewrite_part (part : Ast.part) : Ast.part =
    match part with
    | Comment _ -> part
    | Value ast -> Value (rewrite_ast ast)
    | Keyword _ -> part
    | Group group -> Group (rewrite_group group)
  and rewrite_group_matched_rule
        ({ rule = _; parts; children } : Ast.group)
        (group_rule_parts : Syntax.Rule.part list)
        (into_group_rule : Syntax.Rule.group option)
    : Ast.group
    =
    { rule = into_group_rule
    ; parts = rewrite_parts_matched_rule parts group_rule_parts
    ; children = children |> Tuple.map rewrite_child
    }
  and rewrite_parts_matched_rule
        (parts : Ast.part list)
        (rule_parts : Syntax.Rule.part list)
    : Ast.part list
    =
    Log.trace (fun log ->
      log
        "go_parts %a %a"
        (List.print Ast.Part.print)
        parts
        (List.print Syntax.Rule.Part.print)
        rule_parts);
    match parts, rule_parts with
    | [], [] -> []
    | Value value :: rest_parts, Value _binding :: rest_rule_parts ->
      Value (rewrite_ast value) :: rewrite_parts_matched_rule rest_parts rest_rule_parts
    | Group group :: rest_parts, Group rule_group :: rest_rule_parts ->
      Group (rewrite_group_matched_rule group rule_group.parts (Some rule_group))
      :: rewrite_parts_matched_rule rest_parts rest_rule_parts
    | Keyword _ :: rest_parts, _ -> rewrite_parts_matched_rule rest_parts rule_parts
    | Comment c :: rest_parts, _ ->
      Comment c :: rewrite_parts_matched_rule rest_parts rule_parts
    | _, [] -> fail "value/group not present in target syntax rule"
    | _, first :: rest_rule_parts ->
      let keyword =
        match first with
        | Whitespace _ -> []
        | Keyword k ->
          let token_shape =
            match
              Lexer.read_all Lexer.default_rules { contents = k; uri = Uri.empty }
            with
            | [ token; { shape = Token.Shape.Eof; span = _ } ] -> token.shape
            | tokens ->
              fail "keyword is multiple tokens??? %a" (List.print Token.print) tokens
          in
          [ Ast.Keyword { shape = token_shape; span = Span.beginning_of Uri.empty } ]
        | Value _ | Group _ -> fail "unmatched value/group in target syntax rule"
      in
      keyword @ rewrite_parts_matched_rule parts rest_rule_parts
  in
  rewrite_ast ast
;;

let rec rewrite_all rewrite ast =
  match rewrite with
  | [] -> ast
  | first :: rest -> rewrite_all rest (rewrite_one first ast)
;;

let run : Args.t -> unit =
  fun { paths; hl_output; rewrite; inplace } ->
  let paths =
    match paths with
    | [] -> [ Uri.stdin ]
    | _ -> paths
  in
  paths
  |> List.iter (fun path ->
    let source = Source.read path in
    let ruleset = Kast_default_syntax.ruleset in
    let parsed = Parser.parse source ruleset in
    let rewritten =
      { parsed with
        ast =
          (let rewritten = rewrite_all rewrite parsed.ast in
           Log.trace (fun log -> log "original  ast: %a" Ast.print parsed.ast);
           Log.trace (fun log -> log "rewritten ast: %a" Ast.print rewritten);
           rewritten)
      }
    in
    rewritten |> Kast_fmt.format Format.str_formatter;
    let formatted = Format.flush_str_formatter () in
    if inplace
    then (
      let out = open_out (path |> Uri.path) in
      output_string out formatted;
      close_out out)
    else (
      match hl_output with
      | Some output ->
        let parsed =
          Parser.parse
            { contents = formatted; uri = Uri.of_string "ocaml:formatted" }
            ruleset
        in
        let highlight_print =
          match output with
          | Term -> (module Term : Output)
          | Html -> (module Html : Output)
        in
        Kast_highlight.print highlight_print Format.std_formatter parsed
      | None -> Format.printf "%s" formatted))
;;
