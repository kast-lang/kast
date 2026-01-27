open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast.T

let parse_wrap_mode : Lexer.t -> Syntax.Rule.wrap_mode option =
  fun lexer ->
  if lexer |> Lexer.peek |> Token.is_raw "@wrap"
  then (
    lexer |> Lexer.advance;
    let token = Lexer.next lexer in
    Some
      (match token.shape with
       | Ident { raw = "if_any"; _ } -> Syntax.Rule.WrapMode.IfAnyNonAssociative
       | Ident { raw = "if_any_assoc"; _ } -> Syntax.Rule.WrapMode.IfAnyAssociative
       | Ident { raw = "always"; _ } -> Syntax.Rule.WrapMode.Always
       | Ident { raw = "never"; _ } -> Syntax.Rule.WrapMode.Never
       | _ -> fail "Expected wrap mode, got %a" Token.print token))
  else None
;;

let parse : Lexer.t -> Syntax.rule =
  fun lexer ->
  let start = (Lexer.peek lexer).span.start in
  let do_parse =
    if lexer |> Lexer.peek |> Token.is_raw "@no_parse"
    then (
      lexer |> Lexer.advance;
      false)
    else true
  in
  let get_name (token : Token.t) =
    match token.shape with
    | Ident { raw; _ } -> raw
    | String { contents; _ } -> contents
    | _ -> fail "Expected rule name, got %a" Token.print token
  in
  let name = get_name (Lexer.next lexer) in
  let priority =
    let token = Lexer.next lexer in
    try Token.Shape.as_float token.shape with
    | Invalid_argument _ -> fail "Expected rule priority, got %a" Token.print token
  in
  let wrap_mode =
    parse_wrap_mode lexer
    |> Option.unwrap_or_else (fun () ->
      fail "expected wrap mode, got %a" Token.print (Lexer.peek lexer))
  in
  (let token = Lexer.next lexer in
   if token |> Token.is_raw "=" |> not
   then fail "Expected \"=\", got %a" Token.print token);
  let rec collect_parts () : Syntax.Rule.part list =
    let rec part ?(left_assoc = false) () : Syntax.Rule.part option =
      let token = Lexer.peek lexer in
      match token.shape with
      | Punct { raw = "<-"; _ } when not left_assoc ->
        Lexer.advance lexer;
        part ~left_assoc:true ()
      | String { contents; _ } when not left_assoc ->
        if String.is_whitespace contents
        then (
          Lexer.advance lexer;
          let nowrap = contents in
          let wrap : string =
            if lexer |> Lexer.peek |> Token.is_raw "/"
            then (
              Lexer.advance lexer;
              let token = Lexer.next lexer in
              match token.shape with
              | String { contents; _ } -> contents
              | _ -> fail "Expected wrap str, got %a" Token.print token)
            else nowrap
          in
          Some (Whitespace { nowrap; wrap }))
        else (
          Lexer.advance lexer;
          Some (Keyword contents))
      | Punct { raw = "("; _ } ->
        fail "flat groups are broken";
        Some (group_part Syntax.Rule.Flat)
      | Ident { raw; _ } ->
        Lexer.advance lexer;
        let name = if raw = "_" then None else Some raw in
        let peek = Lexer.peek lexer in
        if peek |> Token.is_raw "="
        then (
          (* group *) Lexer.advance lexer;
          Some (group_part (Syntax.Rule.Nested { name })))
        else (* Not group *)
          (
          let priority =
            if left_assoc
            then Syntax.Rule.Priority.GreaterOrEqual
            else (
              match Token.raw peek with
              | Some "->" ->
                Lexer.advance lexer;
                Syntax.Rule.Priority.GreaterOrEqual
              | Some ":" ->
                Lexer.advance lexer;
                let peek = Lexer.peek lexer in
                if peek |> Token.is_raw "any"
                then (
                  Lexer.advance lexer;
                  Syntax.Rule.Priority.Filter Any)
                else if peek |> Token.is_raw ">=" || peek |> Token.is_raw ">"
                then (
                  let f =
                    match peek |> Token.raw with
                    | Some ">=" ->
                      (fun f : Syntax.Rule.Priority.filter -> GreaterOrEqual f)
                    | Some ">" -> (fun f : Syntax.Rule.Priority.filter -> Greater f)
                    | _ -> unreachable "???"
                  in
                  Lexer.advance lexer;
                  let priority_token = Lexer.peek lexer in
                  let priority =
                    try
                      let parsed = Token.Shape.as_float priority_token.shape in
                      Lexer.advance lexer;
                      parsed
                    with
                    | Invalid_argument _ ->
                      Error.error
                        priority_token.span
                        "Expected priority, got %a"
                        Token.print
                        priority_token;
                      0.0
                  in
                  Syntax.Rule.Priority.Filter (f priority))
                else (
                  Error.error
                    peek.span
                    "Expected priority filter, got %a"
                    Token.print
                    peek;
                  Syntax.Rule.Priority.Greater)
              | _ ->
                (* defaulting to Greater means we only need
                      to annotate with <- or -> where we want associativity
                      or :any where we want parentheses-like behavior *)
                Syntax.Rule.Priority.Greater)
          in
          Some (Value { name; priority }))
      | _ ->
        if left_assoc
        then Error.error token.span "Expected value name, got %a" Token.print token;
        None
    and group_part nested =
      Lexer.expect_next lexer "(";
      let wrap_mode = parse_wrap_mode lexer in
      let parts = collect_parts () in
      Lexer.expect_next lexer ")";
      let peek = Lexer.peek lexer in
      let quantifier =
        match peek |> Token.raw with
        | Some "?" ->
          Lexer.advance lexer;
          Some Syntax.Rule.Optional
        | _ -> None
      in
      Group { id = Id.gen (); wrap_mode; nested; parts; quantifier }
    in
    match part () with
    | Some part -> part :: collect_parts ()
    | None -> []
  in
  let finish = Lexer.position lexer in
  { id = Id.gen ()
  ; span = { start; finish; uri = (Lexer.source lexer).uri }
  ; name
  ; priority
  ; parts = collect_parts ()
  ; do_parse
  ; wrap_mode
  }
;;

type collect_result =
  { parsed : Ast.part list
  ; children : (string option * Ast.child) list
  ; remaining : Parsed_part.t list
  ; span : span option
  }

let merge_spans (a : span) (b : span option) : span option =
  Some
    (match b with
     | None -> a
     | Some b -> { uri = a.uri; start = a.start; finish = b.finish })
;;

let collect : Parsed_part.t list -> Syntax.Rule.t -> Ast.t =
  fun parts rule ->
  Log.trace (fun log -> log "Collecting %d parts into %s" (List.length parts) rule.name);
  Log.trace (fun log -> log "parts = %a" (List.print Parsed_part.print) parts);
  let rec collect_children
            (parsed_parts : Parsed_part.t list)
            (rule_parts : Syntax.Rule.part list)
    : collect_result
    =
    match parsed_parts, rule_parts with
    | Comment comment :: parsed_parts_tail, _ ->
      let { parsed; children; remaining; span = parsed_span } =
        collect_children parsed_parts_tail rule_parts
      in
      { parsed = Comment comment :: parsed
      ; children
      ; remaining
      ; span = merge_spans comment.span parsed_span
      }
    | _, Whitespace _ :: rule_parts_tail -> collect_children parsed_parts rule_parts_tail
    | ( Keyword parsed_keyword :: parsed_parts_tail
      , Keyword expected_keyword :: rule_parts_tail ) ->
      if parsed_keyword |> Token.is_raw expected_keyword |> not
      then
        unreachable
          "Can't collect parsed parts, expected keyword %a, got %a"
          String.print_debug
          expected_keyword
          Token.print
          parsed_keyword;
      let { parsed; children; remaining; span = parsed_span } =
        collect_children parsed_parts_tail rule_parts_tail
      in
      { parsed = Keyword parsed_keyword :: parsed
      ; children
      ; remaining
      ; span = merge_spans parsed_keyword.span parsed_span
      }
    | Value _ :: _, Keyword _ :: _ -> unreachable "matched value & keyword %s" __LOC__
    | Keyword _ :: _, Value _ :: _ -> unreachable "matched keyword & value %s" __LOC__
    | Value value :: parsed_parts_tail, Value binding :: rule_parts_tail ->
      let { parsed; children; remaining; span } =
        collect_children parsed_parts_tail rule_parts_tail
      in
      { parsed = Value value :: parsed
      ; children = (binding.name, Ast value) :: children
      ; remaining
      ; span = merge_spans value.data span
      }
    | _, Group group_rule :: rule_parts_tail ->
      let go_in =
        match group_rule.quantifier with
        | None -> true
        | Some Optional ->
          let expected_keyword =
            group_rule.parts
            |> List.find_map (function
              | Syntax.Rule.Whitespace _ -> None
              | Syntax.Rule.Keyword keyword -> Some keyword
              | _ -> fail "Optional groups should have keyword as first part")
            |> Option.get
          in
          (match parsed_parts with
           | Keyword keyword :: _ when keyword |> Token.is_raw expected_keyword -> true
           | _ -> false)
      in
      (match go_in with
       | false -> collect_children parsed_parts rule_parts_tail
       | true ->
         let { parsed = group_parsed
             ; children = group_children
             ; span = group_span
             ; remaining
             }
           =
           collect_children parsed_parts group_rule.parts
         in
         let group_span = group_span |> Option.get in
         let { parsed; children; remaining; span = parsed_span } =
           collect_children remaining rule_parts_tail
         in
         let group : Ast.group =
           { rule = Some group_rule
           ; parts = group_parsed
           ; children = group_children |> Tuple.of_list
           ; span = group_span
           }
         in
         { parsed = Group group :: parsed
         ; children =
             (match group_rule.nested with
              | Flat -> group_children @ children
              | Nested { name } -> (name, Group group) :: children)
         ; remaining
         ; span = merge_spans group_span parsed_span
         })
    | _, [] -> { parsed = []; children = []; remaining = parsed_parts; span = None }
    | [], _ -> failwith "not enough values supplied"
  in
  let { parsed; children; remaining; span } = collect_children parts rule.parts in
  let span = span |> Option.get in
  if remaining |> List.length <> 0 then fail "too many values supplied";
  { shape =
      Ast.Complex
        { rule
        ; root =
            { rule = None; parts = parsed; children = children |> Tuple.of_list; span }
        }
  ; data = span
  }
;;
