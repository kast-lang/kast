open Std
open Kast_util
open Kast_highlight
module Parser = Kast_parser
module Syntax = Kast_syntax

module Args = struct
  type rewrite =
    { from : Uri.t
    ; into : Uri.t
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
      { rest with
        rewrite = { from = Uri.file from; into = Uri.file into } :: rest.rewrite
      }
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

let parse_ruleset uri = Parser.Ruleset.parse_lines (Source.read uri).contents

let rewrite_one ({ from = _; into } : Args.rewrite) (ast : Ast.t) : Ast.t =
  let into = parse_ruleset into in
  let rec rewrite_ast (ast : Ast.t) : Ast.t =
    Log.trace (fun log -> log "rewriting %a" Ast.print ast);
    match ast.shape with
    | Ast.Empty -> ast
    | Ast.Simple _ -> ast
    | Ast.Complex { rule; root } ->
      (match into |> Parser.Ruleset.find_rule_opt rule.name with
       | Some rule_into ->
         (try
            { shape =
                Ast.Complex
                  { rule = rule_into
                  ; root = rewrite_group_matched_rule root rule_into.parts None
                  }
            ; span = ast.span
            }
          with
          | e ->
            Log.error (fun log ->
              log "while rewriting %S at %a" rule.name Span.print ast.span);
            raise e)
       | None ->
         { shape = Ast.Complex { rule; root = rewrite_group root }; span = ast.span })
    | Ast.Syntax node ->
      { shape =
          Ast.Syntax
            { node with value_after = node.value_after |> Option.map rewrite_ast }
      ; span = ast.span
      }
    | Ast.Error _ -> ast
  and rewrite_group ({ rule; parts; children; span } : Ast.group) : Ast.group =
    { rule
    ; parts = parts |> List.map rewrite_part
    ; children = children |> Tuple.map rewrite_child
    ; span
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
        ({ rule = _; parts; children; span } : Ast.group)
        (group_rule_parts : Syntax.Rule.part list)
        (into_group_rule : Syntax.Rule.group option)
    : Ast.group
    =
    let children = children |> Tuple.map rewrite_child in
    { rule = into_group_rule
    ; parts = rewrite_parts_matched_rule 0 children parts group_rule_parts
    ; children
    ; span
    }
  and rewrite_parts_matched_rule
        (unnamed_child_idx : int)
        (children : Ast.child tuple)
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
    let unnamed_child_idx = ref unnamed_child_idx in
    let get_member : string option -> Tuple.member =
      fun name ->
      match name with
      | Some name -> Name name
      | None ->
        let member = Tuple.Member.Index !unnamed_child_idx in
        unnamed_child_idx := !unnamed_child_idx + 1;
        member
    in
    match parts, rule_parts with
    | [], [] -> []
    | Comment c :: rest_parts, _ ->
      Comment c
      :: rewrite_parts_matched_rule !unnamed_child_idx children rest_parts rule_parts
    | _, Value binding :: rest_rule_parts ->
      let member = get_member binding.name in
      let value =
        children
        |> Tuple.get_opt member
        |> Option.unwrap_or_else (fun () -> fail "%a not found" Tuple.Member.print member)
        |> Ast.Child.expect_ast
      in
      Value value
      :: rewrite_parts_matched_rule !unnamed_child_idx children parts rest_rule_parts
    | _, Group rule_group :: rest_rule_parts ->
      (match rule_group.nested with
       | Flat -> fail "fl"
       | Nested { name } ->
         let member = get_member name in
         let expanded : Ast.part list =
           match rule_group.quantifier with
           | Some Optional ->
             (match children |> Tuple.get_opt member with
              | Some child ->
                let group = child |> Ast.Child.expect_group in
                [ Group
                    (rewrite_group_matched_rule group rule_group.parts (Some rule_group))
                ]
              | None -> [])
           | None ->
             let group = children |> Tuple.get member |> Ast.Child.expect_group in
             [ Group (rewrite_group_matched_rule group rule_group.parts (Some rule_group))
             ]
         in
         expanded
         @ rewrite_parts_matched_rule !unnamed_child_idx children parts rest_rule_parts)
    | Keyword _ :: rest_parts, _ ->
      rewrite_parts_matched_rule !unnamed_child_idx children rest_parts rule_parts
    | Value _ :: rest_parts, _ | Group _ :: rest_parts, _ ->
      rewrite_parts_matched_rule !unnamed_child_idx children rest_parts rule_parts
    | _, Whitespace _ :: rest_rule_parts ->
      rewrite_parts_matched_rule !unnamed_child_idx children parts rest_rule_parts
    | _, Keyword k :: rest_rule_parts ->
      let token_shape =
        match Lexer.read_all Lexer.default_rules { contents = k; uri = Uri.empty } with
        | [ token; { shape = Token.Shape.Eof; span = _ } ] -> token.shape
        | tokens ->
          fail "keyword is multiple tokens??? %a" (List.print Token.print) tokens
      in
      Ast.Keyword { shape = token_shape; span = Span.beginning_of Uri.empty }
      :: rewrite_parts_matched_rule !unnamed_child_idx children parts rest_rule_parts
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
    let ruleset =
      match rewrite with
      | first :: _ -> parse_ruleset first.from
      | _ -> Kast_default_syntax.ruleset
    in
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
