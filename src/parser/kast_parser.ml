open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast

exception Error of (formatter -> unit)

let () =
  Printexc.register_printer (function
    | Error f ->
        eprintln "@{<red>Parse error:@} %a" (fun fmt () -> f fmt) ();
        exit 1
    | _ -> None)

let error : 'never. ('a, formatter, unit, 'never) format4 -> 'a =
 fun format -> Format.kdprintf (fun f -> raise <| Error f) format

let expect_eof : Lexer.t -> unit =
 fun lexer ->
  try Lexer.expect_eof lexer with Lexer.Error f -> raise <| Error f

module Rule = struct
  let parse : Lexer.t -> Syntax.rule =
   fun lexer ->
    let get_name (token : Token.t) =
      match token.shape with
      | Ident { raw; _ } -> raw
      | String { contents; _ } -> contents
      | _ -> error "Expected rule name, got %a" Token.print token
    in
    let name = get_name (Lexer.next lexer) in
    let priority =
      let token = Lexer.next lexer in
      try Token.Shape.as_float token.shape
      with Invalid_argument _ ->
        error "Expected rule priority, got %a" Token.print token
    in
    let wrap_mode =
      let token = Lexer.next lexer in
      if token |> Token.is_raw "wrap" |> not then
        error "Expected \"wrap\", got %a" Token.print token;
      let token = Lexer.next lexer in
      match token.shape with
      | Ident { raw = "if_any"; _ } -> Syntax.Rule.WrapMode.IfAny
      | Ident { raw = "always"; _ } -> Syntax.Rule.WrapMode.Always
      | Ident { raw = "never"; _ } -> Syntax.Rule.WrapMode.Never
      | _ -> error "Expected wrap mode, got %a" Token.print token
    in
    (let token = Lexer.next lexer in
     if token |> Token.is_raw "=" |> not then
       error "Expected \"=\", got %a" Token.print token);
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
                  | _ -> error "Expected wrap str, got %a" Token.print token)
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
                      error "Expected \"any\", got %a" Token.print peek;
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
              error "Expected value name, got %a" Token.print token;
            None
      in
      match part () with
      | Some part -> part :: collect_parts ()
      | None -> []
    in
    { name; priority; parts = collect_parts (); wrap_mode }

  let collect : Ast.part list -> Syntax.Rule.t -> Ast.t =
   fun parts rule ->
    let values =
      parts
      |> List.filter_map (function
           | Ast.Value ast -> Some ast
           | _ -> None)
    in
    let span : span =
      let spans =
        parts
        |> List.filter_map (function
             | Ast.Keyword spanned -> Some spanned.span
             | Ast.Value ast -> Some ast.span
             | Ast.Comment _ -> None)
      in
      {
        start = (spans |> List.head |> fun span -> span.start);
        finish = (spans |> List.last |> fun span -> span.finish);
        filename = (List.head spans).filename;
      }
    in
    Log.trace "Collecting %d values into %s" (List.length values) rule.name;
    Log.trace "Collecting %a" (List.print Ast.print) values;
    let rec collect_children :
        Ast.t list ->
        Syntax.Rule.part list ->
        (Syntax.Rule.binding * Ast.t) list =
     fun values parts ->
      match (values, parts) with
      | _, (Keyword _ | Whitespace _) :: parts_tail ->
          collect_children values parts_tail
      | value :: values_tail, Value binding :: parts_tail ->
          (binding, value) :: collect_children values_tail parts_tail
      | [], [] -> []
      | [], _ -> failwith "not enough values supplied"
      | _, [] -> failwith "too many values supplied"
    in
    let children = collect_children values rule.parts in
    let children =
      List.fold_left
        (fun tuple (binding, value) ->
          let (* because OCaml is OCaml *) binding : Syntax.Rule.binding =
            binding
          in
          Tuple.add binding.name value tuple)
        Tuple.empty children
    in
    { shape = Ast.Complex { rule; parts; children }; span }
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
              error "Duplicate rule: %a and %a" Syntax.Rule.print existing
                Syntax.Rule.print rule
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
      | first :: rest ->
          let edge : Edge.t =
            match first with
            | Keyword keyword -> Keyword keyword
            | Value _ -> Value
            | Whitespace _ -> unreachable ":)"
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
              | Whitespace _ -> unreachable ":)");
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
               { contents = line; filename = Special "rule" }
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
    let parsed_rev : Ast.part list ref =
      ref (start |> Option.map (fun ast -> Ast.Value ast) |> Option.to_list)
    in
    let count_comments_before () =
      parsed_rev :=
        (!comments_before |> List.rev
        |> List.map (fun comment -> Ast.Comment comment))
        @ !parsed_rev;
      comments_before := []
    in
    let made_progress : unit -> bool =
      let start_index = (Lexer.peek lexer).span.start.index in
      fun () -> (Lexer.peek lexer).span.start.index <> start_index
    in
    let terminate (node : RuleSet.node) : parse_result =
      if made_progress () then
        match node.terminal with
        | Some rule ->
            let parsed = List.rev !parsed_rev in
            let ast = Rule.collect parsed rule in
            Log.trace "Parsed %a" Ast.print ast;
            comments_before := !comments_before @ read_comments lexer;
            MadeProgress ast
        | None -> error "Unexpected %a" Token.print (Lexer.peek lexer)
      else NoProgress
    in
    (* should return bool but we do option because let* makes it easier *)
    let continue_with (node : RuleSet.node) : unit option =
      let* () =
        if made_progress () then Some ()
        else
          (* On the **first** iteration need to check that start value satisfies prev filter *)
          let start_priority : Syntax.Rule.priority option =
            Option.bind start (fun start ->
                match start.shape with
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
    let rec go (node : RuleSet.node) : parse_result =
      comments_before := !comments_before @ read_comments lexer;
      let token = Lexer.peek lexer in
      let raw_token = Token.raw token in
      let+ () =
        (* try to follow with token as a keyword *)
        let* keyword = raw_token in
        let* () =
          if continuation_keywords |> StringSet.contains keyword then None
          else Some ()
        in
        let edge : RuleSet.edge = Keyword keyword in
        let* next = RuleSet.EdgeMap.find_opt edge node.next in
        let* () = continue_with next in
        count_comments_before ();
        parsed_rev := Keyword token :: !parsed_rev;
        Log.trace "Followed with keyword %S" keyword;
        Lexer.advance lexer;
        Some (go next)
      in
      let+ () =
        (* try to follow with a value *)
        let* () =
          (* actually don't if we are just starting *)
          match made_progress () || start |> Option.is_some with
          | true -> Some ()
          | false -> None
        in
        let edge : RuleSet.edge = Value in
        let* next = RuleSet.EdgeMap.find_opt edge node.next in
        let* () = continue_with next in
        let inner_continuation_keywords =
          match next.value_filter with
          | None | Some Any -> StringSet.empty
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
        Some (go next)
      in
      terminate node
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
        error "expected \";\" to finish syntax, got %a" Token.print token;
      let tokens : Token.t list = Lexer.stop_rec tokens_rec in
      let span : span =
        {
          start = (List.head tokens).span.start;
          finish = (List.last tokens).span.start;
          filename = (List.head tokens).span.filename;
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

    let start = start |> Option.or_else parse_simple in
    let result =
      match start with
      | Some start -> (
          if made_progress () then MadeProgress start
          else
            match RuleSet.EdgeMap.find_opt Value ruleset.root.next with
            | None -> NoProgress (* no rules starting with a value *)
            | Some node -> go node)
      | None -> go ruleset.root
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
