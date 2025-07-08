open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast

let parse : Lexer.t -> Syntax.rule =
 fun lexer ->
  let start = (Lexer.peek lexer).span.start in
  let get_name (token : Token.t) =
    match token.shape with
    | Ident { raw; _ } -> raw
    | String { contents; _ } -> contents
    | _ -> fail "Expected rule name, got %a" Token.print token
  in
  let name = get_name (Lexer.next lexer) in
  let priority =
    let token = Lexer.next lexer in
    try Token.Shape.as_float token.shape
    with Invalid_argument _ ->
      fail "Expected rule priority, got %a" Token.print token
  in
  let wrap_mode =
    let token = Lexer.next lexer in
    if token |> Token.is_raw "wrap" |> not then
      fail "Expected \"wrap\", got %a" Token.print token;
    let token = Lexer.next lexer in
    match token.shape with
    | Ident { raw = "if_any"; _ } -> Syntax.Rule.WrapMode.IfAny
    | Ident { raw = "always"; _ } -> Syntax.Rule.WrapMode.Always
    | Ident { raw = "never"; _ } -> Syntax.Rule.WrapMode.Never
    | _ -> fail "Expected wrap mode, got %a" Token.print token
  in
  (let token = Lexer.next lexer in
   if token |> Token.is_raw "=" |> not then
     fail "Expected \"=\", got %a" Token.print token);
  let rec collect_parts () : Syntax.Rule.part list =
    let rec part ?(left_assoc = false) () : Syntax.Rule.part option =
      let token = Lexer.peek lexer in
      match token.shape with
      | Punct { raw = "<-"; _ } when not left_assoc ->
          Lexer.advance lexer;
          part ~left_assoc:true ()
      | String { contents; _ } when not left_assoc ->
          if String.is_whitespace contents then (
            Lexer.advance lexer;
            let nowrap = contents in
            let wrap : string =
              if lexer |> Lexer.peek |> Token.is_raw "/" then (
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
      | Ident { raw; _ } when raw <> "syntax" ->
          Lexer.advance lexer;
          let name = if raw = "_" then None else Some raw in
          let peek = Lexer.peek lexer in
          if peek |> Token.is_raw "=" then (
            (* group *)
            Lexer.advance lexer;
            Lexer.expect_next lexer "(";
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
            Some (Group { name; parts; quantifier }))
          else (* Not group *)
            let priority =
              if left_assoc then Syntax.Rule.Priority.GreaterOrEqual
              else
                match Token.raw peek with
                | Some "->" ->
                    Lexer.advance lexer;
                    Syntax.Rule.Priority.GreaterOrEqual
                | Some ":" ->
                    Lexer.advance lexer;
                    let peek = Lexer.peek lexer in
                    if peek |> Token.is_raw "any" |> not then
                      Error.error peek.span "Expected \"any\", got %a"
                        Token.print peek;
                    Lexer.advance lexer;
                    Syntax.Rule.Priority.Any
                | _ ->
                    (* defaulting to Greater means we only need
                      to annotate with <- or -> where we want associativity
                      or :any where we want parentheses-like behavior *)
                    Syntax.Rule.Priority.Greater
            in
            Some (Value { name; priority })
      | _ ->
          if left_assoc then
            Error.error token.span "Expected value name, got %a" Token.print
              token;
          None
    in
    match part () with
    | Some part -> part :: collect_parts ()
    | None -> []
  in
  let finish = Lexer.position lexer in
  {
    id = Id.gen ();
    span = { start; finish; uri = (Lexer.source lexer).uri };
    name;
    priority;
    parts = collect_parts ();
    wrap_mode;
  }

type collect_result = {
  parsed : Ast.part list;
  children : (string option * Ast.child) list;
  remaining : Parsed_part.t list;
}

let collect : Parsed_part.t list -> Syntax.Rule.t -> Ast.t =
 fun parts rule ->
  let span : span =
    let spans =
      parts
      |> List.filter_map (function
           | Parsed_part.Keyword spanned -> Some spanned.span
           | Parsed_part.Value ast -> Some ast.span
           | Parsed_part.Comment _ -> None)
    in
    {
      start = (spans |> List.head |> fun span -> span.start);
      finish = (spans |> List.last |> fun span -> span.finish);
      uri = (List.head spans).uri;
    }
  in
  Log.trace "Collecting %d parts into %s" (List.length parts) rule.name;
  Log.trace "parts = %a" (List.print Parsed_part.print) parts;
  let rec collect_children (parsed_parts : Parsed_part.t list)
      (rule_parts : Syntax.Rule.part list) : collect_result =
    match (parsed_parts, rule_parts) with
    | Comment comment :: parsed_parts_tail, _ ->
        let { parsed; children; remaining } =
          collect_children parsed_parts_tail rule_parts
        in
        { parsed = Comment comment :: parsed; children; remaining }
    | _, Whitespace _ :: rule_parts_tail ->
        collect_children parsed_parts rule_parts_tail
    | ( Keyword parsed_keyword :: parsed_parts_tail,
        Keyword expected_keyword :: rule_parts_tail ) ->
        if parsed_keyword |> Token.is_raw expected_keyword |> not then
          unreachable "Can't collect parsed parts, expected keyword %S, got %a"
            expected_keyword Token.print parsed_keyword;
        let { parsed; children; remaining } =
          collect_children parsed_parts_tail rule_parts_tail
        in
        { parsed = Keyword parsed_keyword :: parsed; children; remaining }
    | Value _ :: _, Keyword _ :: _ ->
        unreachable "matched value & keyword %s" __LOC__
    | Keyword _ :: _, Value _ :: _ ->
        unreachable "matched keyword & value %s" __LOC__
    | Value value :: parsed_parts_tail, Value binding :: rule_parts_tail ->
        let { parsed; children; remaining } =
          collect_children parsed_parts_tail rule_parts_tail
        in
        {
          parsed = Value value :: parsed;
          children = (binding.name, Ast value) :: children;
          remaining;
        }
    | _, Group group_rule :: rule_parts_tail -> (
        let go_in =
          match group_rule.quantifier with
          | None -> true
          | Some Optional -> (
              let expected_keyword =
                group_rule.parts
                |> List.find_map (function
                     | Syntax.Rule.Whitespace _ -> None
                     | Syntax.Rule.Keyword keyword -> Some keyword
                     | _ ->
                         fail
                           "Optional groups should have keyword as first part")
                |> Option.get
              in
              match parsed_parts with
              | Keyword keyword :: _
                when keyword |> Token.is_raw expected_keyword ->
                  true
              | _ -> false)
        in
        match go_in with
        | false -> collect_children parsed_parts rule_parts_tail
        | true ->
            let { parsed = group_parsed; children = group_children; remaining }
                =
              collect_children parsed_parts group_rule.parts
            in
            let { parsed; children; remaining } =
              collect_children remaining rule_parts_tail
            in
            let group : Ast.group =
              {
                rule = Some group_rule;
                parts = group_parsed;
                children = group_children |> Tuple.of_list;
              }
            in
            {
              parsed = Group group :: parsed;
              children = (group_rule.name, Group group) :: children;
              remaining;
            })
    | _, [] -> { parsed = []; children = []; remaining = parsed_parts }
    | [], _ -> failwith "not enough values supplied"
  in
  let { parsed; children; remaining } = collect_children parts rule.parts in
  if remaining |> List.length <> 0 then fail "too many values supplied";
  {
    shape =
      Ast.Complex
        {
          rule;
          root =
            {
              rule = None;
              parts = parsed;
              children = children |> Tuple.of_list;
            };
        };
    span;
  }
