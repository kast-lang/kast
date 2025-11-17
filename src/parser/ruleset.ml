open Std
open Kast_util
module Syntax = Kast_syntax
module Lexer = Kast_lexer

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
            Error.error rule.span "Duplicate rule: %a and %a" Syntax.Rule.print
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
            insert ~prev_prev ~prev (group_parts @ rest) inserted_without_group)
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

let find_rule_opt : string -> ruleset -> Syntax.rule option =
 fun name ruleset -> StringMap.find_opt name ruleset.rules

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
         Lexer.expect_eof lexer;
         rule)
  |> of_list

let parse_lines : string -> ruleset =
 fun s ->
  s |> String.split_on_char '\n'
  |> List.filter (fun s ->
         not (String.is_whitespace s || String.starts_with ~prefix:"#" s))
  |> List.map (fun s -> String.strip_prefix ~prefix:"@syntax " s |> Option.get)
  |> List.map (fun s -> String.strip_suffix ~suffix:";" s |> Option.get)
  |> List.filter (fun line -> String.trim line <> "from_scratch")
  |> parse_list
