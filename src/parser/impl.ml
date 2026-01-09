open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Ast = Kast_ast
module Syntax = Kast_syntax

let expect_eof : Lexer.t -> unit =
  fun lexer ->
  try Lexer.expect_eof lexer with
  | Lexer.Error msg ->
    Error.error
      { start = Lexer.position lexer
      ; finish = Lexer.position lexer
      ; uri = (Lexer.source lexer).uri
      }
      "%t"
      msg
;;

type parse_result =
  | MadeProgress of Ast.t
  | NoProgress

type parse_one_state =
  { parsed_rev : Parsed_part.t list
  ; node : Ruleset.node
  ; made_progress : bool
  }

type context =
  { unused_comments_rev : Token.comment list ref
  ; lexer : Lexer.t
  ; ruleset : Ruleset.t
  ; continuation_keywords : StringSet.t
  ; filter : Syntax.Rule.Priority.filter
  }

let rec peek_token (context : context) : Token.t =
  let peek = context.lexer |> Lexer.peek in
  match peek.shape with
  | Comment comment ->
    context.unused_comments_rev
    := { shape = comment; span = peek.span } :: !(context.unused_comments_rev);
    context.lexer |> Lexer.advance;
    peek_token context
  | _ -> peek
;;

let rec _unused = ()

and take_comments_rev (context : context) : Token.comment list =
  let result = !(context.unused_comments_rev) in
  context.unused_comments_rev := [];
  result

and take_comments (context : context) : Token.comment list =
  context |> take_comments_rev |> List.rev

and take_comments_as_parts_rev (context : context) : Parsed_part.t list =
  context |> take_comments_rev |> List.map (fun comment -> Parsed_part.Comment comment)

(* On the **first** iteration need to check that start value satisfies prev filter *)
and is_trying_to_extend_incorrect_priority
      (next_node : Ruleset.node)
      (state : parse_one_state)
      (_context : context)
  : bool
  =
  with_return (fun { return } ->
    if state.made_progress then return false;
    let start_value =
      match state.parsed_rev with
      | [ Value value ] -> value
      | _ -> return false
    in
    let rec priority (value : Ast.t) : Syntax.Rule.priority option =
      match value.shape with
      | Ast.Error _ -> None
      | Ast.Simple _ -> None
      | Ast.Complex { rule; _ } -> Some rule.priority
      | Ast.Syntax { value_after; _ } -> value_after |> Option.and_then priority
    in
    match priority start_value with
    | None -> false
    | Some start_priority ->
      Syntax.Rule.Priority.check_filter
        start_priority
        (next_node.prev_value_filter |> Option.get)
      |> not)

and should_continue_with
      (next_node : Ruleset.node)
      (state : parse_one_state)
      (context : context)
  : bool
  =
  if is_trying_to_extend_incorrect_priority next_node state context
  then false
  else (
    match next_node.priority_range with
    | None -> false
    | Some range -> Syntax.Rule.Priority.check_filter_with_range range context.filter)

and try_continue_with_keyword (state : parse_one_state) (context : context)
  : parse_result option
  =
  Log.trace (fun log -> log "trying to continue with keyword");
  with_return (fun { return } ->
    let token = context |> peek_token in
    let* keyword = token |> Token.raw in
    if
      (not state.made_progress)
      && context.continuation_keywords |> StringSet.contains keyword
    then (
      Log.trace (fun log ->
        log "Not continuing with keyword because %S is in continuation keywords" keyword);
      return None);
    let edge : Ruleset.edge = Keyword keyword in
    let* next_node = state.node.next |> Ruleset.EdgeMap.find_opt edge in
    if not (should_continue_with next_node state context) then return None;
    Log.trace (fun log -> log "Following with keyword %S" keyword);
    Lexer.advance context.lexer;
    let new_state : parse_one_state =
      { node = next_node
      ; parsed_rev =
          [ Parsed_part.Keyword token ]
          @ (context |> take_comments_as_parts_rev)
          @ state.parsed_rev
      ; made_progress = true
      }
    in
    Some (parse_one_from new_state context))

and try_continue_with_value (state : parse_one_state) (context : context)
  : parse_result option
  =
  with_return (fun { return } ->
    if state.parsed_rev |> List.is_empty then return None;
    Log.trace (fun log -> log "trying to continue with value");
    let edge : Ruleset.edge = Value in
    let* next_node = state.node.next |> Ruleset.EdgeMap.find_opt edge in
    if not (should_continue_with next_node state context) then return None;
    let inner_context : context =
      { lexer = context.lexer
      ; ruleset = context.ruleset
      ; filter = next_node.value_filter |> Option.get
      ; continuation_keywords =
          (match next_node.value_filter with
           | None | Some Any -> next_node.next_keywords
           | _ -> StringSet.union context.continuation_keywords next_node.next_keywords)
      ; unused_comments_rev = context.unused_comments_rev
      }
    in
    Log.trace (fun log -> log "Trying to parse value to follow with");
    let* value : Ast.t = parse_value inner_context in
    Log.trace (fun log -> log "Following with value %a" Ast.print value);
    let new_state : parse_one_state =
      { made_progress = true
      ; parsed_rev = Value value :: state.parsed_rev
      ; node = next_node
      }
    in
    Some (parse_one_from new_state context))

and terminate (state : parse_one_state) (context : context) : parse_result =
  Log.trace (fun log -> log "trying to terminate");
  with_return (fun { return } ->
    if not state.made_progress then return NoProgress;
    (match state.node.terminal with
     | Some rule ->
       let parsed = state.parsed_rev |> List.rev in
       let ast = Rule.collect parsed rule in
       Log.trace (fun log -> log "Parsed %a" Ast.print ast);
       return (MadeProgress ast)
     | None -> ());
    (let single_value =
       state.parsed_rev
       |> List.fold_left
            (fun acc part ->
               match part with
               | Parsed_part.Comment _ -> acc
               | Parsed_part.Keyword _ -> Some None
               | Parsed_part.Value value ->
                 (match acc with
                  | None -> Some (Some value)
                  | Some _ -> Some None))
            None
     in
     match single_value with
     | Some (Some ast) -> return (MadeProgress ast)
     | _ -> ());
    let token = context |> peek_token in
    Error.error token.span "Unexpected %a" Token.print token;
    let parts = state.parsed_rev |> List.rev in
    Log.trace (fun log -> log "Parsed: %a" (List.print Parsed_part.print) parts);
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
      { start = (spans |> List.head).start
      ; finish = (spans |> List.last).finish
      ; uri = token.span.uri
      }
    in
    MadeProgress { shape = Error { parts }; span })

(* TODO improve this logic? *)
and try_skipping_token_because_error (state : parse_one_state) (context : context)
  : parse_result option
  =
  with_return (fun { return } ->
    let peek = context |> peek_token in
    (match peek.shape with
     | Token.Shape.Punct _ -> ()
     | Token.Shape.Ident _ -> return None
     | Token.Shape.String _ -> return None
     | Token.Shape.Number _ -> return None
     | Token.Shape.Comment _ -> unreachable "comment???"
     | Token.Shape.Eof -> return None);
    let* raw_token = peek |> Token.raw in
    if context.continuation_keywords |> StringSet.contains raw_token then return None;
    if context.ruleset.root.next_keywords |> StringSet.contains raw_token then return None;
    (match
       context.ruleset.root.next
       |> Ruleset.EdgeMap.find Value
       |> fun node -> node.next |> Ruleset.EdgeMap.find_opt (Keyword raw_token)
     with
     | Some node ->
       (match state.node.priority_range, node.prev_value_filter with
        | Some range, Some filter
          when Syntax.Rule.Priority.check_filter_with_range range filter ->
          return None
        | _ -> ())
     | None -> ());
    Error.error peek.span "Skipping unexpected %a" Token.print peek;
    context.lexer |> Lexer.advance;
    Some (parse_one_from state context))

and parse_one_from (state : parse_one_state) (context : context) : parse_result =
  try_continue_with_keyword state context
  |> Option.or_else (fun () -> try_continue_with_value state context)
  |> Option.or_else (fun () -> try_skipping_token_because_error state context)
  |> Option.unwrap_or_else (fun () -> terminate state context)

and parse_simple (context : context) : Ast.t option =
  with_return (fun { return } ->
    let peek = context |> peek_token in
    let start = peek.span.start in
    let shape =
      match peek.shape with
      | Number _ ->
        Ast.Simple { comments_before = context |> take_comments; token = peek }
      | String _ ->
        Ast.Simple { comments_before = context |> take_comments; token = peek }
      | Punct { raw = "@syntax"; _ } -> return <| Some (parse_syntax_extension context)
      | Ident { raw; _ } ->
        if Ruleset.is_keyword raw context.ruleset
        then return None
        else Ast.Simple { comments_before = context |> take_comments; token = peek }
      | Comment _ -> unreachable "comments were skipped"
      | Eof -> return None
      | Punct _ -> return None
    in
    Log.trace (fun log -> log "Parsed simple %a" Ast.Shape.print shape);
    context.lexer |> Lexer.advance;
    Some ({ shape; span = { peek.span with start } } : Ast.t))

and parse_minimal (context : context) : parse_result =
  match parse_simple context with
  | Some simple -> MadeProgress simple
  | None ->
    parse_one_from
      { made_progress = false; parsed_rev = []; node = context.ruleset.root }
      context

and extend_minimal (start_value : Ast.t) (context : context) : parse_result =
  match context.ruleset.root.next |> Ruleset.EdgeMap.find_opt Value with
  | None -> NoProgress
  | Some node ->
    let state : parse_one_state =
      { made_progress = false; parsed_rev = [ Value start_value ]; node }
    in
    parse_one_from state context

and parse_or_extend_minimal (start_value : Ast.t option) (context : context)
  : parse_result
  =
  match start_value with
  | Some value -> extend_minimal value context
  | None -> parse_minimal context

and parse_syntax_extension (context : context) : Ast.t =
  let tokens_rec = context.lexer |> Lexer.start_rec in
  let token = context.lexer |> Lexer.next in
  if token |> Token.is_raw "@syntax" |> not then fail "expected \"@syntax\"";
  let comments_before = context |> take_comments in
  let mode =
    match context |> peek_token |> Token.raw with
    | Some "from_scratch" ->
      context.lexer |> Lexer.advance;
      Ast.SyntaxMode.FromScratch
    | _ -> Ast.SyntaxMode.Define (Rule.parse context.lexer)
  in
  let token = context.lexer |> Lexer.next in
  if token |> Token.is_raw ";" |> not
  then Error.error token.span "expected \";\" to finish syntax, got %a" Token.print token;
  let tokens : Token.t list = Lexer.stop_rec tokens_rec in
  let span : span =
    { start = (List.head tokens).span.start
    ; finish = (List.last tokens).span.start
    ; uri = (List.head tokens).span.uri
    }
  in
  let new_ruleset =
    match mode with
    | Define rule -> Ruleset.add rule context.ruleset
    | FromScratch -> Ruleset.empty
  in
  let value_after : Ast.t option = parse_value { context with ruleset = new_ruleset } in
  let shape : Ast.shape = Syntax { comments_before; mode; value_after; tokens } in
  let span =
    match value_after with
    | None -> span
    | Some value -> { span with finish = value.span.finish }
  in
  { shape; span }

and parse_value (context : context) : Ast.t option =
  let rec loop ~(already_parsed : Ast.t option) : Ast.t option =
    match parse_or_extend_minimal already_parsed context with
    | MadeProgress ast ->
      Log.trace (fun log -> log "Made progress: %a" Ast.print ast);
      loop ~already_parsed:(Some ast)
    | NoProgress -> already_parsed
  in
  loop ~already_parsed:None
;;
