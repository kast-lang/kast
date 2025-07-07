open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast
module Error = Error

type error = Error.t

let error = Error.error

let expect_eof : Lexer.t -> unit =
 fun lexer ->
  try Lexer.expect_eof lexer
  with Lexer.Error msg ->
    Effect.perform
    <| Error.Error
         {
           msg;
           span =
             {
               start = Lexer.position lexer;
               finish = Lexer.position lexer;
               uri = (Lexer.source lexer).uri;
             };
         }

type parsed_part =
  | Comment of Token.comment
  | Keyword of Token.t
  | Value of Ast.t

module ParsedPart = struct
  let print : formatter -> parsed_part -> unit =
   fun fmt -> function
    | Comment comment ->
        fprintf fmt "comment %a" Token.Shape.print (Comment comment.shape)
    | Keyword token -> Token.print fmt token
    | Value ast -> Ast.print fmt ast
end

module Rule = struct
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
                        error peek.span "Expected \"any\", got %a" Token.print
                          peek;
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
              error token.span "Expected value name, got %a" Token.print token;
            None
      in
      match part () with
      | Some part -> part :: collect_parts ()
      | None -> []
    in
    let finish = Lexer.position lexer in
    {
      span = { start; finish; uri = (Lexer.source lexer).uri };
      name;
      priority;
      parts = collect_parts ();
      wrap_mode;
    }

  type collect_result = {
    parsed : Ast.part list;
    children : (string option * Ast.child) list;
    remaining : parsed_part list;
  }

  let collect : parsed_part list -> Syntax.Rule.t -> Ast.t =
   fun parts rule ->
    let span : span =
      let spans =
        parts
        |> List.filter_map (function
             | Keyword spanned -> Some spanned.span
             | Value ast -> Some ast.span
             | Comment _ -> None)
      in
      {
        start = (spans |> List.head |> fun span -> span.start);
        finish = (spans |> List.last |> fun span -> span.finish);
        uri = (List.head spans).uri;
      }
    in
    Log.trace "Collecting %d parts into %s" (List.length parts) rule.name;
    Log.trace "parts = %a" (List.print ParsedPart.print) parts;
    let rec collect_children (parsed_parts : parsed_part list)
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
            unreachable
              "Can't collect parsed parts, expected keyword %S, got %a"
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
                  match group_rule.parts with
                  | Keyword keyword :: _ -> keyword
                  | _ ->
                      fail "Optional groups should have keyword as first part"
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
              let {
                parsed = group_parsed;
                children = group_children;
                remaining;
              } =
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
end

module RuleSet = struct
  module Edge = struct
    type t =
      | Keyword of string
      | Value
    [@@deriving eq, ord]
  end

  type edge = Edge.t

  (* TODO maybe hashmap *)
  module EdgeMap = Map.Make (Edge)

  type node = {
    terminal : Syntax.rule option;
    value_filter : Syntax.Rule.Priority.filter option;
    prev_value_filter : Syntax.Rule.Priority.filter option;
    priority_range : Syntax.Rule.priority Range.inclusive option;
    next : node EdgeMap.t;
    next_keywords : StringSet.t;
  }

  module Node = struct
    let empty : node =
      {
        terminal = None;
        value_filter = None;
        prev_value_filter = None;
        priority_range = None;
        next = EdgeMap.empty;
        next_keywords = StringSet.empty;
      }
  end

  type ruleset = {
    rules : Syntax.rule StringMap.t;
    keywords : StringSet.t;
    root : node;
  }

  type t = ruleset

  let empty : ruleset =
    { rules = StringMap.empty; keywords = StringSet.empty; root = Node.empty }

  let update_value_filter (rule : Syntax.rule) (part : Syntax.Rule.part option)
      current_filter =
    match part with
    | Some (Value { priority; _ }) ->
        let priority_filter =
          Syntax.Rule.Priority.make_filter priority rule.priority
        in
        Some
          (match current_filter with
          | Some current ->
              Syntax.Rule.Priority.stricter_filter current priority_filter
          | None -> priority_filter)
    | _ -> current_filter

  let add : Syntax.rule -> ruleset -> ruleset =
   fun rule ruleset ->
    let rec insert ~(prev_prev : Syntax.Rule.part option)
        ~(prev : Syntax.Rule.part option) (parts : Syntax.Rule.part list)
        (node : node) : node =
      let updated_value_filter =
        update_value_filter rule prev node.value_filter
      in
      let updated_prev_value_filter =
        update_value_filter rule prev_prev node.prev_value_filter
      in
      let updated_priority_range =
        Some
          (let point = Range.Inclusive.point rule.priority in
           Option.map_or point
             (Range.Inclusive.unite Syntax.Rule.Priority.compare point)
             node.priority_range)
      in
      match parts with
      | [] -> (
          match node.terminal with
          | Some existing ->
              error rule.span "Duplicate rule: %a and %a" Syntax.Rule.print
                existing Syntax.Rule.print rule;
              node
          | None ->
              {
                terminal = Some rule;
                value_filter = updated_value_filter;
                prev_value_filter = updated_prev_value_filter;
                priority_range = updated_priority_range;
                next = node.next;
                next_keywords = node.next_keywords;
              })
      | Whitespace _ :: rest -> insert ~prev_prev ~prev rest node
      | Group { name = _; parts = group_parts; quantifier } :: rest -> (
          match quantifier with
          | None -> insert ~prev_prev ~prev (group_parts @ rest) node
          | Some Optional ->
              let inserted_without_group = insert ~prev_prev ~prev rest node in
              insert ~prev_prev ~prev (group_parts @ rest)
                inserted_without_group)
      | first :: rest ->
          let edge : Edge.t =
            match first with
            | Keyword keyword -> Keyword keyword
            | Value _ -> Value
            | Group _ | Whitespace _ -> unreachable ":)"
          in
          let merge_next : node option -> node option =
           fun current ->
            let next =
              match current with
              | None -> Node.empty
              | Some existing -> existing
            in
            Some (insert ~prev_prev:prev ~prev:(Some first) rest next)
          in
          {
            terminal = node.terminal;
            value_filter = updated_value_filter;
            prev_value_filter = updated_prev_value_filter;
            priority_range = updated_priority_range;
            next = EdgeMap.update edge merge_next node.next;
            next_keywords =
              (match first with
              | Value _ -> node.next_keywords
              | Keyword keyword -> StringSet.add keyword node.next_keywords
              | Group _ | Whitespace _ -> unreachable ":)");
          }
    in
    {
      rules = StringMap.add rule.name rule ruleset.rules;
      keywords = StringSet.add_seq (Syntax.Rule.keywords rule) ruleset.keywords;
      root = insert ~prev_prev:None ~prev:None rule.parts ruleset.root;
    }

  let is_keyword : string -> ruleset -> bool =
   fun word ruleset -> StringSet.contains word ruleset.keywords

  let find_rule : string -> ruleset -> Syntax.rule =
   fun name ruleset -> StringMap.find name ruleset.rules

  let of_list : Syntax.rule list -> ruleset =
   fun rules -> List.fold_right add rules empty

  let parse_list : string list -> ruleset =
   fun rules ->
    rules
    |> List.map (fun line ->
           let lexer =
             Lexer.init Lexer.default_rules
               { contents = line; uri = Uri.of_string "ocaml:parse_list/rule" }
           in
           let rule = Rule.parse lexer in
           expect_eof lexer;
           rule)
    |> of_list

  let parse_lines : string -> ruleset =
   fun s ->
    s |> String.split_on_char '\n'
    |> List.filter (fun s ->
           not (String.is_whitespace s || String.starts_with ~prefix:"#" s))
    |> List.map (fun s -> String.strip_prefix ~prefix:"syntax " s |> Option.get)
    |> List.map (fun s -> String.strip_suffix ~suffix:";" s |> Option.get)
    |> List.filter (fun line -> String.trim line <> "from_scratch")
    |> parse_list
end

type ruleset = RuleSet.t
type parser = { mutable ruleset : ruleset }

let init : ruleset -> parser = fun ruleset -> { ruleset }

let add_rule : Syntax.rule -> parser -> unit =
 fun rule parser -> parser.ruleset <- RuleSet.add rule parser.ruleset

let rec read_comments lexer : Token.comment list =
  let peek = Lexer.peek lexer in
  match peek.shape with
  | Comment comment ->
      Lexer.advance lexer;
      { shape = comment; span = peek.span } :: read_comments lexer
  | _ -> []

module Impl = struct
  type parse_result =
    | MadeProgress of Ast.t
    | NoProgress

  let rec parse_one :
      comments_before:Token.comment list ref ->
      start:Ast.t option ->
      ruleset ->
      continuation_keywords:StringSet.t ->
      filter:Syntax.Rule.Priority.filter ->
      Lexer.t ->
      parse_result =
   fun ~comments_before ~start ruleset ~continuation_keywords ~filter lexer ->
    (match start with
    | None -> Log.trace "Start to parse one"
    | Some _ -> Log.trace "Start to parse one (having value)");
    let parsed_rev : parsed_part list ref =
      ref (start |> Option.map (fun ast -> Value ast) |> Option.to_list)
    in
    let count_comments_before () =
      parsed_rev :=
        (!comments_before |> List.rev
        |> List.map (fun comment -> Comment comment))
        @ !parsed_rev;
      comments_before := []
    in
    let made_progress = ref false in
    let terminate (node : RuleSet.node) : parse_result =
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
                     | Comment _ -> acc
                     | Keyword _ -> Some None
                     | Value value -> (
                         match acc with
                         | None -> Some (Some value)
                         | Some _ -> Some None))
                   None
            in
            match single_value with
            | Some (Some ast) -> MadeProgress ast
            | _ ->
                let token = Lexer.peek lexer in
                error token.span "Unexpected %a" Token.print token;
                let parts = !parsed_rev |> List.rev in
                Log.error "Parsed: %a" (List.print ParsedPart.print) parts;
                let spans =
                  parts
                  |> List.map (function
                       | Comment comment -> comment.span
                       | Value value -> value.span
                       | Keyword keyword -> keyword.span)
                in
                let parts =
                  parts
                  |> List.map (function
                       | Comment comment -> Ast.Comment comment
                       | Value value -> Ast.Value value
                       | Keyword keyword -> Ast.Keyword keyword)
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
    let continue_with (node : RuleSet.node) : unit option =
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
    let rec go ~(used_keyword : bool) (node : RuleSet.node) : parse_result =
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
        let edge : RuleSet.edge = Keyword keyword in
        let* next = RuleSet.EdgeMap.find_opt edge node.next in
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
        let edge : RuleSet.edge = Value in
        let* next = RuleSet.EdgeMap.find_opt edge node.next in
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
                 && ruleset.root.next |> RuleSet.EdgeMap.find Value
                    |> fun node ->
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
        error token.span "Skipping unexpected %a" Token.print token;
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
        error token.span "expected \";\" to finish syntax, got %a" Token.print
          token;
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
        | Define rule -> RuleSet.add rule ruleset
        | FromScratch -> RuleSet.empty
      in
      let value_after : Ast.t option =
        parse new_ruleset ~comments_before:(ref []) ~continuation_keywords
          ~filter lexer
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
                  (Ast.Simple
                     { comments_before = !comments_before; token = peek })
            | String _ ->
                Some
                  (Ast.Simple
                     { comments_before = !comments_before; token = peek })
            | Ident { raw = "syntax"; _ } ->
                return <| Some (parse_syntax_extension ())
            | Ident { raw; _ } ->
                if RuleSet.is_keyword raw ruleset then None
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
            match RuleSet.EdgeMap.find_opt Value ruleset.root.next with
            | None -> NoProgress (* no rules starting with a value *)
            | Some node -> go ~used_keyword:false node)
      | None -> go ~used_keyword:false ruleset.root
    in

    (match result with
    | MadeProgress _ -> Log.trace "Finished parse one (made progress)"
    | NoProgress -> Log.trace "Finished parse one (no progress)");
    result

  and parse :
      ruleset ->
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
end

type result = {
  ast : Ast.t option;
  trailing_comments : Token.comment list;
  eof : position;
}

let parse_with_lexer : Lexer.t -> ruleset -> result =
 fun lexer ruleset ->
  let comments_before = ref [] in
  let result =
    Impl.parse ruleset ~comments_before ~continuation_keywords:StringSet.empty
      ~filter:Any lexer
  in
  expect_eof lexer;
  {
    ast = result;
    trailing_comments = !comments_before;
    eof = Lexer.position lexer;
  }

let parse : source -> ruleset -> result =
 fun source ruleset ->
  parse_with_lexer (Lexer.init Lexer.default_rules source) ruleset
