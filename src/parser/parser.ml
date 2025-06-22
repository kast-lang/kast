open Std
open Util

module Rule = struct
  module Priority = struct
    type t = float
    type priority = t
    type filter = Greater of priority | GreaterOrEqual of priority | Any

    let compare = Float.compare

    let stricter_filter a b =
      match (a, b) with
      | Any, p | p, Any -> p
      | Greater a, Greater b -> Greater (Float.max a b)
      | GreaterOrEqual a, GreaterOrEqual b -> GreaterOrEqual (Float.max a b)
      | Greater a, GreaterOrEqual b | GreaterOrEqual b, Greater a ->
          if a >= b then Greater a else GreaterOrEqual b
  end

  type priority = Priority.t
  type binding = { name : string option; priority : Priority.filter }
  type part = Keyword of string | Value of binding
  type rule = { name : string; priority : float; parts : part list }
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
    Log.trace "@[<v>Collecting %a@]" (List.print Ast.print) values;
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
end

type rule = Rule.t

module RuleSet = struct
  module Edge = struct
    type t = Keyword of string | Value [@@deriving eq, ord]
  end

  type edge = Edge.t

  (* TODO maybe hashmap *)
  module EdgeMap = Map.Make (Edge)

  type node = {
    terminal : rule option;
    value_filter : Rule.Priority.filter option;
    priority_range : Rule.priority Range.inclusive option;
    next : node EdgeMap.t;
  }

  module Node = struct
    let empty : node =
      {
        terminal = None;
        value_filter = None;
        priority_range = None;
        next = EdgeMap.empty;
      }
  end

  module StringSet = Set.Make (String)

  type ruleset = { keywords : StringSet.t; root : node }
  type t = ruleset

  let empty : ruleset = { keywords = StringSet.empty; root = Node.empty }

  let add : rule -> ruleset -> ruleset =
   fun rule ruleset ->
    let rec insert ~(prev : Rule.part option) (parts : Rule.part list)
        (node : node) : node =
      let updated_value_filter =
        match prev with
        | None | Some (Keyword _) -> node.value_filter
        | Some (Value { priority; _ }) ->
            Some
              (match node.value_filter with
              | Some current -> Rule.Priority.stricter_filter current priority
              | None -> priority)
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
              failwith
              @@ make_string "Duplicate rule: %a and %a" Rule.print existing
                   Rule.print rule
          | None ->
              {
                terminal = Some rule;
                value_filter = updated_value_filter;
                priority_range = updated_priority_range;
                next = node.next;
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
            Some (insert ~prev:(Some first) rest next)
          in
          {
            terminal = node.terminal;
            value_filter = updated_value_filter;
            priority_range = updated_priority_range;
            next = EdgeMap.update edge merge_next node.next;
          }
    in
    {
      keywords = StringSet.add_seq (Rule.keywords rule) ruleset.keywords;
      root = insert ~prev:None rule.parts ruleset.root;
    }

  let is_keyword : string -> ruleset -> bool =
   fun word ruleset -> StringSet.contains word ruleset.keywords

  let of_list : rule list -> ruleset =
   fun rules -> List.fold_right add rules empty
end

type ruleset = RuleSet.t
type parser = { mutable ruleset : ruleset }

let init : ruleset -> parser = fun ruleset -> { ruleset }

let add_rule : rule -> parser -> unit =
 fun rule parser -> parser.ruleset <- RuleSet.add rule parser.ruleset

module Impl = struct
  type parse_result = MadeProgress of Ast.t | NoProgress
  type parsed = Keyword of Lexer.token spanned | Value of Ast.t

  let rec parse_one :
      start:Ast.t option ->
      ruleset ->
      Rule.Priority.filter ->
      Lexer.t ->
      parse_result =
   fun ~start ruleset filter lexer ->
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
      match node.terminal with
      | Some rule ->
          if made_progress () then
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
          else NoProgress
      | None ->
          if made_progress () then
            failwith
            @@ make_string "Could not finish parsing, peek=%a" Lexer.Token.print
                 (Lexer.peek lexer).value
          else NoProgress
    in
    let continue_with (node : RuleSet.node) : unit option =
      let* range = node.priority_range in
      let should_continue =
        match filter with
        | Any -> true
        | Greater x -> range.max > x
        | GreaterOrEqual x -> range.max >= x
      in
      if should_continue then Some () else None
    in
    let rec go (node : RuleSet.node) : parse_result =
      let spanned = Lexer.peek lexer in
      let token = spanned.value in
      let raw_token = Lexer.Token.raw token in
      let+ () =
        (* try to follow with token as a keyword *)
        let* raw_token = raw_token in
        let edge : RuleSet.edge = Keyword raw_token in
        let* next = RuleSet.EdgeMap.find_opt edge node.next in
        let* () = continue_with next in
        parsed_rev := Keyword spanned :: !parsed_rev;
        Log.trace "Followed with keyword %S" raw_token;
        Lexer.skip lexer;
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
        let* value : Ast.t =
          parse ruleset (next.value_filter |> Option.get) lexer
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
      Lexer.skip lexer;
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

  and parse : ruleset -> Rule.Priority.filter -> Lexer.t -> Ast.t option =
   fun ruleset filter lexer ->
    let rec loop (already_parsed : Ast.t option) =
      match parse_one ~start:already_parsed ruleset filter lexer with
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
  Impl.parse ruleset Any lexer
