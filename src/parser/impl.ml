open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Ast = Kast_ast
module Syntax = Kast_syntax

let expect_eof : Lexer.t -> unit =
 fun lexer ->
  try Lexer.expect_eof lexer
  with Lexer.Error msg ->
    Error.error
      {
        start = Lexer.position lexer;
        finish = Lexer.position lexer;
        uri = (Lexer.source lexer).uri;
      }
      "%t" msg

let rec read_comments lexer : Token.comment list =
  let peek = Lexer.peek lexer in
  match peek.shape with
  | Comment comment ->
      Lexer.advance lexer;
      { shape = comment; span = peek.span } :: read_comments lexer
  | _ -> []

type parse_result =
  | MadeProgress of Ast.t
  | NoProgress

let rec parse_one :
    comments_before:Token.comment list ref ->
    start:Ast.t option ->
    Ruleset.t ->
    continuation_keywords:StringSet.t ->
    filter:Syntax.Rule.Priority.filter ->
    Lexer.t ->
    parse_result =
 fun ~comments_before ~start ruleset ~continuation_keywords ~filter lexer ->
  (match start with
  | None -> Log.trace "Start to parse one"
  | Some _ -> Log.trace "Start to parse one (having value)");
  let parsed_rev : Parsed_part.t list ref =
    ref
      (start |> Option.map (fun ast -> Parsed_part.Value ast) |> Option.to_list)
  in
  let count_comments_before () =
    parsed_rev :=
      (!comments_before |> List.rev
      |> List.map (fun comment -> Parsed_part.Comment comment))
      @ !parsed_rev;
    comments_before := []
  in
  let made_progress = ref false in
  let terminate (node : Ruleset.node) : parse_result =
    if !made_progress then
      match node.terminal with
      | Some rule ->
          let parsed = List.rev !parsed_rev in
          let ast = Rule.collect parsed rule in
          Log.trace "Parsed %a" Ast.print ast;
          comments_before := !comments_before @ read_comments lexer;
          MadeProgress ast
      | None -> (
          let single_value =
            !parsed_rev
            |> List.fold_left
                 (fun acc part ->
                   match part with
                   | Parsed_part.Comment _ -> acc
                   | Parsed_part.Keyword _ -> Some None
                   | Parsed_part.Value value -> (
                       match acc with
                       | None -> Some (Some value)
                       | Some _ -> Some None))
                 None
          in
          match single_value with
          | Some (Some ast) -> MadeProgress ast
          | _ ->
              let token = Lexer.peek lexer in
              Error.error token.span "Unexpected %a" Token.print token;
              let parts = !parsed_rev |> List.rev in
              Log.error "Parsed: %a" (List.print Parsed_part.print) parts;
              let spans =
                parts
                |> List.map (function
                     | Parsed_part.Comment comment -> comment.span
                     | Parsed_part.Value value -> value.span
                     | Parsed_part.Keyword keyword -> keyword.span)
              in
              let parts =
                parts
                |> List.map (function
                     | Parsed_part.Comment comment -> Ast.Comment comment
                     | Parsed_part.Value value -> Ast.Value value
                     | Parsed_part.Keyword keyword -> Ast.Keyword keyword)
              in
              let span : span =
                {
                  start = (spans |> List.head).start;
                  finish = (spans |> List.last).finish;
                  uri = token.span.uri;
                }
              in
              MadeProgress { shape = Error { parts }; span })
    else NoProgress
  in
  (* should return bool but we do option because let* makes it easier *)
  let continue_with (node : Ruleset.node) : unit option =
    let* () =
      if !made_progress then Some ()
      else
        (* On the **first** iteration need to check that start value satisfies prev filter *)
        let start_priority : Syntax.Rule.priority option =
          Option.bind start (fun start ->
              match start.shape with
              | Ast.Error _ -> None
              | Ast.Simple _ -> None
              | Ast.Complex { rule; _ } -> Some rule.priority
              | Ast.Syntax _ -> None (* TODO check *))
        in
        match start_priority with
        | None -> Some ()
        | Some start_priority ->
            Syntax.Rule.Priority.check_filter start_priority
              (node.prev_value_filter |> Option.get)
            |> Bool.then_some ()
    in
    let* range = node.priority_range in
    Syntax.Rule.Priority.check_filter_with_range range filter
    |> Bool.then_some ()
  in
  let rec go ~(used_keyword : bool) (node : Ruleset.node) : parse_result =
    comments_before := !comments_before @ read_comments lexer;
    let token = Lexer.peek lexer in
    let raw_token = Token.raw token in
    let+ () =
      (* try to follow with token as a keyword *)
      let* keyword = raw_token in
      let* () =
        if
          (not used_keyword)
          && continuation_keywords |> StringSet.contains keyword
        then None
        else Some ()
      in
      let edge : Ruleset.edge = Keyword keyword in
      let* next = Ruleset.EdgeMap.find_opt edge node.next in
      let* () = continue_with next in
      count_comments_before ();
      parsed_rev := Keyword token :: !parsed_rev;
      Log.trace "Followed with keyword %S" keyword;
      made_progress := true;
      Lexer.advance lexer;
      Some (go ~used_keyword:true next)
    in
    let+ () =
      (* try to follow with a value *)
      let* () =
        (* actually don't if we are just starting *)
        match !made_progress || start |> Option.is_some with
        | true -> Some ()
        | false -> None
      in
      let edge : Ruleset.edge = Value in
      let* next = Ruleset.EdgeMap.find_opt edge node.next in
      let* () = continue_with next in
      let inner_continuation_keywords =
        match next.value_filter with
        | None | Some Any -> next.next_keywords
        | _ -> StringSet.union continuation_keywords next.next_keywords
      in
      let* value : Ast.t =
        parse ruleset ~comments_before
          ~continuation_keywords:inner_continuation_keywords
          ~filter:(next.value_filter |> Option.get)
          lexer
      in
      parsed_rev := Value value :: !parsed_rev;
      Log.trace "Followed with value %a" Ast.print value;
      made_progress := true;
      Some (go ~used_keyword next)
    in
    let token = Lexer.peek lexer in
    let raw_token = Token.raw token in
    let should_skip_token_because_error =
      let x =
        raw_token
        |> Option.map_or false (fun raw ->
               continuation_keywords |> StringSet.contains raw |> not
               && ruleset.root.next_keywords |> StringSet.contains raw |> not
               && ruleset.root.next |> Ruleset.EdgeMap.find Value |> fun node ->
                  node.next_keywords |> StringSet.contains raw |> not)
      in
      match token.shape with
      | Token.Shape.Punct _ -> x
      | Token.Shape.Ident _ -> false
      | Token.Shape.String _ -> false
      | Token.Shape.Number _ -> false
      | Token.Shape.Comment _ -> unreachable "comment???"
      | Token.Shape.Eof -> false
    in
    if should_skip_token_because_error then (
      Error.error token.span "Skipping unexpected %a" Token.print token;
      (* Log.error "continuation keywords: %a"
          (List.print String.print_dbg)
          (continuation_keywords |> StringSet.to_list); *)
      Lexer.advance lexer;
      go ~used_keyword node)
    else terminate node
  in
  let parse_syntax_extension () : Ast.t =
    let tokens_rec = Lexer.start_rec lexer in
    let token = Lexer.next lexer in
    if token |> Token.is_raw "syntax" |> not then fail "expected \"syntax\"";
    let mode =
      match Lexer.peek lexer |> Token.raw with
      | Some "from_scratch" ->
          Lexer.advance lexer;
          Ast.SyntaxMode.FromScratch
      | _ -> Ast.SyntaxMode.Define (Rule.parse lexer)
    in
    let token = Lexer.next lexer in
    if token |> Token.is_raw ";" |> not then
      Error.error token.span "expected \";\" to finish syntax, got %a"
        Token.print token;
    let tokens : Token.t list = Lexer.stop_rec tokens_rec in
    let span : span =
      {
        start = (List.head tokens).span.start;
        finish = (List.last tokens).span.start;
        uri = (List.head tokens).span.uri;
      }
    in
    let new_ruleset =
      match mode with
      | Define rule -> Ruleset.add rule ruleset
      | FromScratch -> Ruleset.empty
    in
    let value_after : Ast.t option =
      parse new_ruleset ~comments_before:(ref []) ~continuation_keywords ~filter
        lexer
    in
    let shape : Ast.shape =
      Syntax { comments_before = !comments_before; mode; value_after; tokens }
    in
    let span =
      match value_after with
      | None -> span
      | Some value -> { span with finish = value.span.finish }
    in
    { shape; span }
  in
  let parse_simple () : Ast.t option =
    with_return (fun { return } ->
        let start = (Lexer.peek lexer).span.start in
        comments_before := !comments_before @ read_comments lexer;
        let peek = Lexer.peek lexer in
        let* shape =
          match peek.shape with
          | Eof -> None
          | Punct _ -> None
          | Number _ ->
              Some
                (Ast.Simple { comments_before = !comments_before; token = peek })
          | String _ ->
              Some
                (Ast.Simple { comments_before = !comments_before; token = peek })
          | Ident { raw = "syntax"; _ } ->
              return <| Some (parse_syntax_extension ())
          | Ident { raw; _ } ->
              if Ruleset.is_keyword raw ruleset then None
              else
                Some
                  (Ast.Simple
                     { comments_before = !comments_before; token = peek })
          | Comment _ -> unreachable "comments were skipped"
        in
        comments_before := [];
        Lexer.advance lexer;
        comments_before := !comments_before @ read_comments lexer;
        Some ({ shape; span = { peek.span with start } } : Ast.t))
  in

  let start =
    start
    |> Option.or_else (fun () ->
           let simple = parse_simple () in
           (match simple with
           | Some _ -> made_progress := true
           | None -> ());
           simple)
  in
  let result =
    match start with
    | Some start -> (
        if !made_progress then MadeProgress start
        else
          match Ruleset.EdgeMap.find_opt Value ruleset.root.next with
          | None -> NoProgress (* no rules starting with a value *)
          | Some node -> go ~used_keyword:false node)
    | None -> go ~used_keyword:false ruleset.root
  in

  (match result with
  | MadeProgress _ -> Log.trace "Finished parse one (made progress)"
  | NoProgress -> Log.trace "Finished parse one (no progress)");
  result

and parse :
    Ruleset.t ->
    comments_before:Token.comment list ref ->
    continuation_keywords:StringSet.t ->
    filter:Syntax.Rule.Priority.filter ->
    Lexer.t ->
    Ast.t option =
 fun ruleset ~comments_before ~continuation_keywords ~filter lexer ->
  let rec loop (already_parsed : Ast.t option) =
    match
      parse_one ~comments_before ~start:already_parsed ruleset
        ~continuation_keywords ~filter lexer
    with
    | MadeProgress ast ->
        Log.trace "Made progress: %a" Ast.print ast;
        loop (Some ast)
    | NoProgress -> already_parsed
  in
  loop None
