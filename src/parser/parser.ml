open Std
open Util

exception Error of (formatter -> unit)

let error : 'never. ('a, formatter, unit, 'never) format4 -> 'a =
 fun format -> Format.kdprintf (fun f -> raise @@ Error f) format

let expect_eof : Lexer.t -> unit =
 fun lexer ->
  try Lexer.expect_eof lexer with Lexer.Error f -> raise @@ Error f

module Rule = struct
  module Priority = struct
    type t = float
    type priority = t

    type filter =
      | Greater of priority
      | GreaterOrEqual of priority
      | Any

    type filter_kind =
      | Greater
      | GreaterOrEqual
      | Any

    let compare = Float.compare

    let stricter_filter (a : filter) (b : filter) =
      match (a, b) with
      | Any, p | p, Any -> p
      | Greater a, Greater b -> Greater (Float.max a b)
      | GreaterOrEqual a, GreaterOrEqual b -> GreaterOrEqual (Float.max a b)
      | Greater a, GreaterOrEqual b | GreaterOrEqual b, Greater a ->
          if a >= b then Greater a else GreaterOrEqual b

    let make_filter : filter_kind -> priority -> filter =
     fun kind p ->
      match kind with
      | Greater -> Greater p
      | GreaterOrEqual -> GreaterOrEqual p
      | Any -> Any

    let check_filter_with_range (range : priority Range.Inclusive.t)
        (filter : filter) =
      match filter with
      | Any -> true
      | Greater x -> range.max > x
      | GreaterOrEqual x -> range.max >= x

    let check_filter (p : priority) (filter : filter) =
      match filter with
      | Any -> true
      | Greater x -> p > x
      | GreaterOrEqual x -> p >= x
  end

  type priority = Priority.t

  type binding = {
    name : string option;
    priority : Priority.filter_kind;
  }

  type part =
    | Keyword of string
    | Value of binding

  type rule = {
    name : string;
    priority : float;
    parts : part list;
  }

  type t = rule

  let print : formatter -> rule -> unit =
   fun fmt rule -> fprintf fmt "%S" rule.name

  let keywords : rule -> string Seq.t =
   fun rule ->
    List.to_seq rule.parts
    |> Seq.filter_map (function
         | Keyword keyword -> Some keyword
         | Value _ -> None)

  let collect : Ast.t list -> rule -> Ast.kind =
   fun values rule ->
    Log.trace "Collecting %d values into %s" (List.length values) rule.name;
    Log.trace "Collecting %a" (List.print Ast.print) values;
    let rec collect : Ast.t list -> part list -> (binding * Ast.t) list =
     fun values parts ->
      match (values, parts) with
      | _, Keyword _ :: parts_tail -> collect values parts_tail
      | value :: values_tail, Value binding :: parts_tail ->
          (binding, value) :: collect values_tail parts_tail
      | [], [] -> []
      | [], _ -> failwith "not enough values supplied"
      | _, [] -> failwith "too many values supplied"
    in
    let collected = collect values rule.parts in
    let children =
      List.fold_left
        (fun tuple (binding, value) ->
          let (* because OCaml is OCaml *) binding : binding = binding in
          Tuple.add binding.name value tuple)
        Tuple.empty collected
    in
    Ast.Complex { name = rule.name; children }

  let parse : Lexer.t -> rule =
   fun lexer ->
    let get_name (spanned : Lexer.token spanned) =
      match spanned.value with
      | Ident { raw; _ } -> raw
      | String { contents; _ } -> contents
      | _ ->
          error "Expected rule name, got %a"
            (Spanned.print Lexer.Token.print)
            spanned
    in
    let name = get_name (Lexer.next lexer) in
    let priority =
      let token = Lexer.next lexer in
      try Lexer.Token.as_float token.value
      with Invalid_argument _ ->
        error "Expected rule priority, got %a"
          (Spanned.print Lexer.Token.print)
          token
    in
    (let token = Lexer.next lexer in
     if Lexer.Token.raw token.value <> Some "=" then
       error "Expected \"=\", got %a" (Spanned.print Lexer.Token.print) token);
    let rec collect_parts () : part list =
      let rec part ?(left_assoc = false) () : part option =
        let token = Lexer.peek lexer in
        match token.value with
        | Punct { raw = "<-"; _ } when not left_assoc ->
            Lexer.advance lexer;
            part ~left_assoc:true ()
        | String { contents; _ } when not left_assoc ->
            Lexer.advance lexer;
            Some (Keyword contents)
        | Ident { raw; _ } ->
            Lexer.advance lexer;
            let name = if raw = "_" then None else Some raw in
            let peek = Lexer.peek lexer in
            let priority =
              if left_assoc then Priority.GreaterOrEqual
              else
                match Lexer.Token.raw peek.value with
                | Some "->" ->
                    Lexer.advance lexer;
                    Priority.GreaterOrEqual
                | Some ":" ->
                    Lexer.advance lexer;
                    let peek = Lexer.peek lexer in
                    if Lexer.Token.raw peek.value <> Some "any" then
                      error "Expected \"any\", got %a"
                        (Spanned.print Lexer.Token.print)
                        peek;
                    Lexer.advance lexer;
                    Priority.Any
                | _ ->
                    (* defaulting to Greater means we only need
                      to annotate with <- or -> where we want associativity
                      or :any where we want parentheses-like behavior *)
                    Priority.Greater
            in
            Some (Value { name; priority })
        | _ ->
            if left_assoc then
              error "Expected value name, got %a"
                (Spanned.print Lexer.Token.print)
                token;
            None
      in
      match part () with
      | Some part -> part :: collect_parts ()
      | None -> []
    in
    { name; priority; parts = collect_parts () }
end

type rule = Rule.t

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
    terminal : rule option;
    value_filter : Rule.Priority.filter option;
    prev_value_filter : Rule.Priority.filter option;
    priority_range : Rule.priority Range.inclusive option;
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
    rules : rule StringMap.t;
    keywords : StringSet.t;
    root : node;
  }

  type t = ruleset

  let empty : ruleset =
    { rules = StringMap.empty; keywords = StringSet.empty; root = Node.empty }

  let update_value_filter (rule : rule) (part : Rule.part option) current_filter
      =
    match part with
    | None | Some (Keyword _) -> current_filter
    | Some (Value { priority; _ }) ->
        let priority_filter =
          Rule.Priority.make_filter priority rule.priority
        in
        Some
          (match current_filter with
          | Some current ->
              Rule.Priority.stricter_filter current priority_filter
          | None -> priority_filter)

  let add : rule -> ruleset -> ruleset =
   fun rule ruleset ->
    let rec insert ~(prev_prev : Rule.part option) ~(prev : Rule.part option)
        (parts : Rule.part list) (node : node) : node =
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
             (Range.Inclusive.unite Rule.Priority.compare point)
             node.priority_range)
      in
      match parts with
      | [] -> (
          match node.terminal with
          | Some existing ->
              error "Duplicate rule: %a and %a" Rule.print existing Rule.print
                rule
          | None ->
              {
                terminal = Some rule;
                value_filter = updated_value_filter;
                prev_value_filter = updated_prev_value_filter;
                priority_range = updated_priority_range;
                next = node.next;
                next_keywords = node.next_keywords;
              })
      | first :: rest ->
          let edge : Edge.t =
            match first with
            | Keyword keyword -> Keyword keyword
            | Value _ -> Value
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
              | Keyword keyword -> StringSet.add keyword node.next_keywords);
          }
    in
    {
      rules = StringMap.add rule.name rule ruleset.rules;
      keywords = StringSet.add_seq (Rule.keywords rule) ruleset.keywords;
      root = insert ~prev_prev:None ~prev:None rule.parts ruleset.root;
    }

  let is_keyword : string -> ruleset -> bool =
   fun word ruleset -> StringSet.contains word ruleset.keywords

  let find_rule : string -> ruleset -> rule =
   fun name ruleset -> StringMap.find name ruleset.rules

  let of_list : rule list -> ruleset =
   fun rules -> List.fold_right add rules empty

  let parse_list : string list -> ruleset =
   fun rules ->
    rules
    |> List.map (fun line ->
           let lexer =
             Lexer.init Lexer.default_rules
               { contents = line; filename = "<rule>" }
           in
           let rule = Rule.parse lexer in
           expect_eof lexer;
           rule)
    |> of_list

  let parse_lines : string -> ruleset =
   fun s ->
    s |> String.split_on_char '\n'
    |> List.filter (fun line ->
           (not (String.starts_with ~prefix:"#" line))
           && not (String.is_whitespace line))
    |> parse_list
end

type ruleset = RuleSet.t
type parser = { mutable ruleset : ruleset }

let init : ruleset -> parser = fun ruleset -> { ruleset }

let add_rule : rule -> parser -> unit =
 fun rule parser -> parser.ruleset <- RuleSet.add rule parser.ruleset

module Impl = struct
  type parse_result =
    | MadeProgress of Ast.t
    | NoProgress

  type parsed =
    | Keyword of Lexer.token spanned
    | Value of Ast.t

  let rec parse_one :
      start:Ast.t option ->
      ruleset ->
      continuation_keywords:StringSet.t ->
      filter:Rule.Priority.filter ->
      Lexer.t ->
      parse_result =
   fun ~start ruleset ~continuation_keywords ~filter lexer ->
    (match start with
    | None -> Log.trace "Start to parse one"
    | Some _ -> Log.trace "Start to parse one (having value)");
    let parsed_rev : parsed list ref =
      ref (start |> Option.map (fun ast -> Value ast) |> Option.to_list)
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
            let values =
              parsed
              |> List.filter_map (function
                   | Keyword _ -> None
                   | Value ast -> Some ast)
            in
            let spans =
              parsed
              |> List.map (function
                   | Keyword spanned -> spanned.span
                   | Value ast -> ast.span)
            in
            MadeProgress
              {
                kind = Rule.collect values rule;
                span =
                  {
                    start = (spans |> List.head |> fun span -> span.start);
                    finish = (spans |> List.last |> fun span -> span.finish);
                    filename = (Lexer.source lexer).filename;
                  };
              }
        | None ->
            error "Unexpected %a"
              (Spanned.print Lexer.Token.print)
              (Lexer.peek lexer)
      else NoProgress
    in
    (* should return bool but we do option because let* makes it easier *)
    let continue_with (node : RuleSet.node) : unit option =
      let* () =
        if made_progress () then Some ()
        else
          (* On the **first** iteration need to check that start value satisfies prev filter *)
          let start_priority : Rule.priority option =
            Option.bind start (fun start ->
                match start.kind with
                | Ast.Simple _ -> None
                | Ast.Complex { name; _ } ->
                    let rule : rule = RuleSet.find_rule name ruleset in
                    Some rule.priority)
          in
          match start_priority with
          | None -> Some ()
          | Some start_priority ->
              Rule.Priority.check_filter start_priority
                (node.prev_value_filter |> Option.get)
              |> Bool.then_some ()
      in
      let* range = node.priority_range in
      Rule.Priority.check_filter_with_range range filter |> Bool.then_some ()
    in
    let rec go (node : RuleSet.node) : parse_result =
      let spanned = Lexer.peek lexer in
      let token = spanned.value in
      let raw_token = Lexer.Token.raw token in
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
        parsed_rev := Keyword spanned :: !parsed_rev;
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
          parse ruleset ~continuation_keywords:inner_continuation_keywords
            ~filter:(next.value_filter |> Option.get)
            lexer
        in
        parsed_rev := Value value :: !parsed_rev;
        Log.trace "Followed with value %a" Ast.print value;
        Some (go next)
      in
      terminate node
    in
    let parse_simple () : Ast.t option =
      Lexer.skip_comments lexer;
      let spanned = Lexer.peek lexer in
      let token = spanned.value in
      let* kind =
        match token with
        | Eof -> None
        | Punct _ -> None
        | Number _ -> Some (Ast.Simple { token })
        | String _ -> Some (Ast.Simple { token })
        | Ident { raw; _ } ->
            if RuleSet.is_keyword raw ruleset then None
            else Some (Ast.Simple { token })
        | Comment _ -> unreachable "comments were skipped"
      in
      Lexer.advance lexer;
      Some ({ kind; span = spanned.span } : Ast.t)
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
      continuation_keywords:StringSet.t ->
      filter:Rule.Priority.filter ->
      Lexer.t ->
      Ast.t option =
   fun ruleset ~continuation_keywords ~filter lexer ->
    let rec loop (already_parsed : Ast.t option) =
      match
        parse_one ~start:already_parsed ruleset ~continuation_keywords ~filter
          lexer
      with
      | MadeProgress ast ->
          Log.trace "Made progress: %a" Ast.print ast;
          loop (Some ast)
      | NoProgress -> already_parsed
    in
    loop None
end

let parse : source -> ruleset -> Ast.t option =
 fun source ruleset ->
  let lexer = Lexer.init Lexer.default_rules source in
  let result =
    Impl.parse ruleset ~continuation_keywords:StringSet.empty ~filter:Any lexer
  in
  expect_eof lexer;
  result
