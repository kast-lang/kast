open Std
open Kast_util
module Syntax = Kast_syntax
module Lexer = Kast_lexer

module Category = struct
  module Edge = struct
    type t =
      | Keyword of string
      | Value
    [@@deriving eq, ord]
  end

  type edge = Edge.t

  (* TODO maybe hashmap *)
  module EdgeMap = Map.Make (Edge)

  type node =
    { terminal : Syntax.rule option
    ; value_category : Syntax.Category.t option
    ; value_filter : Syntax.Rule.Priority.filter option
    ; prev_value_filter : Syntax.Rule.Priority.filter option
    ; priority_range : Syntax.Rule.priority Range.inclusive option
    ; next : node EdgeMap.t
    ; next_keywords : StringSet.t
    ; parent : (node * edge) option
    }

  module Node = struct
    let empty ~parent : node =
      { terminal = None
      ; value_category = None
      ; value_filter = None
      ; prev_value_filter = None
      ; priority_range = None
      ; next = EdgeMap.empty
      ; next_keywords = StringSet.empty
      ; parent
      }
    ;;
  end

  type t =
    { rules : Syntax.rule StringMap.t
    ; keywords : StringSet.t
    ; root : node
    }

  let empty : t =
    { rules = StringMap.empty
    ; keywords = StringSet.empty
    ; root = Node.empty ~parent:None
    }
  ;;

  let update_value_category
        (rule : Syntax.rule)
        (part : Syntax.Rule.part option)
        current_category
    =
    match part with
    | Some (Value { category; _ }) ->
      Some
        (match current_category with
         | Some current ->
           if not (Syntax.Category.equal current category)
           then Error.error rule.span "Must have same category for child nodes";
           category
         | None -> category)
    | _ -> current_category
  ;;

  let update_value_filter
        (rule : Syntax.rule)
        (part : Syntax.Rule.part option)
        current_filter
    =
    match part with
    | Some (Value { priority; _ }) ->
      let priority_filter = Syntax.Rule.Priority.make_filter priority rule.priority in
      Some
        (match current_filter with
         | Some current -> Syntax.Rule.Priority.stricter_filter current priority_filter
         | None -> priority_filter)
    | _ -> current_filter
  ;;

  let rec print_node_path fmt (node : node) =
    match node.parent with
    | Some (parent, edge) ->
      print_node_path fmt parent;
      fprintf fmt " ";
      (match edge with
       | Edge.Keyword k -> fprintf fmt "%a" String.print_debug k
       | Edge.Value -> fprintf fmt "_")
    | None -> fprintf fmt "<root>"
  ;;

  let add : Syntax.rule -> t -> t =
    fun rule ruleset ->
    if ruleset.rules |> StringMap.find_opt rule.name |> Option.is_some
    then fail "Already have rule %a" String.print_debug rule.name;
    let updated_rules = StringMap.add rule.name rule ruleset.rules in
    match rule.do_parse with
    | false -> { ruleset with rules = updated_rules }
    | true ->
      let rec insert
                ~(prev_prev : Syntax.Rule.part option)
                ~(prev : Syntax.Rule.part option)
                (parts : Syntax.Rule.part list)
                (node : node)
        : node
        =
        let updated_value_category =
          update_value_category rule prev node.value_category
        in
        let updated_value_filter = update_value_filter rule prev node.value_filter in
        let updated_prev_value_filter =
          update_value_filter rule prev_prev node.prev_value_filter
        in
        let updated_priority_range =
          Some
            (let point = Range.Inclusive.point rule.priority in
             Option.map_or
               point
               (Range.Inclusive.unite Syntax.Rule.Priority.compare point)
               node.priority_range)
        in
        match parts with
        | [] ->
          (match node.terminal with
           | Some existing ->
             Error.error
               rule.span
               "Duplicate syntax %a: %a and %a"
               print_node_path
               node
               Syntax.Rule.print
               existing
               Syntax.Rule.print
               rule;
             node
           | None ->
             let result =
               { terminal = Some rule
               ; value_category = updated_value_category
               ; value_filter = updated_value_filter
               ; prev_value_filter = updated_prev_value_filter
               ; priority_range = updated_priority_range
               ; next = node.next
               ; next_keywords = node.next_keywords
               ; parent = node.parent
               }
             in
             Log.trace (fun log ->
               log "Added %a syntax: %a" Syntax.Rule.print rule print_node_path result);
             result)
        | Whitespace _ :: rest -> insert ~prev_prev ~prev rest node
        | Group { id = _; wrap_mode = _; nested = _; parts = group_parts; quantifier }
          :: rest ->
          (match quantifier with
           | None -> insert ~prev_prev ~prev (group_parts @ rest) node
           | Some Optional ->
             let inserted_without_group = insert ~prev_prev ~prev rest node in
             insert ~prev_prev ~prev (group_parts @ rest) inserted_without_group)
        | first :: rest ->
          let edge : Edge.t =
            match first with
            | Keyword keyword -> Keyword keyword
            | Value binding -> Value
            | Group _ | Whitespace _ -> unreachable ":)"
          in
          let merge_next : node option -> node option =
            fun current ->
            let next =
              match current with
              | None -> Node.empty ~parent:(Some (node, edge))
              | Some existing -> existing
            in
            Some (insert ~prev_prev:prev ~prev:(Some first) rest next)
          in
          { terminal = node.terminal
          ; value_category = updated_value_category
          ; value_filter = updated_value_filter
          ; prev_value_filter = updated_prev_value_filter
          ; priority_range = updated_priority_range
          ; next = EdgeMap.update edge merge_next node.next
          ; next_keywords =
              (match first with
               | Value _ -> node.next_keywords
               | Keyword keyword -> StringSet.add keyword node.next_keywords
               | Group _ | Whitespace _ -> unreachable ":)")
          ; parent = node.parent
          }
      in
      { rules = updated_rules
      ; keywords = StringSet.add_seq (Syntax.Rule.keywords rule) ruleset.keywords
      ; root = insert ~prev_prev:None ~prev:None rule.parts ruleset.root
      }
  ;;

  let is_keyword : string -> t -> bool =
    fun word ruleset -> StringSet.contains word ruleset.keywords
  ;;

  let find_rule : string -> t -> Syntax.rule =
    fun name ruleset -> StringMap.find name ruleset.rules
  ;;

  let find_rule_opt : string -> t -> Syntax.rule option =
    fun name ruleset -> StringMap.find_opt name ruleset.rules
  ;;
end

module CategoryMap = Map.Make (Syntax.Category)

type t = { categories : Category.t CategoryMap.t }

let empty : t = { categories = CategoryMap.empty }

let add (rule : Syntax.rule) (set : t) : t =
  { categories =
      set.categories
      |> CategoryMap.update rule.category (fun current ->
        let current = current |> Option.unwrap_or_else (fun () -> Category.empty) in
        Some (Category.add rule current))
  }
;;

let of_list : Syntax.rule list -> t = fun rules -> List.fold_right add rules empty

let parse_source : source -> t =
  fun source ->
  let lexer = Lexer.init Lexer.default_rules source in
  let rec go acc : t =
    let peek = Lexer.peek lexer in
    if peek |> Lexer.Token.is_eof
    then acc
    else if peek |> Lexer.Token.is_comment
    then (
      Lexer.advance lexer;
      go acc)
    else if peek |> Lexer.Token.is_raw "@syntax"
    then (
      Lexer.advance lexer;
      if lexer |> Lexer.peek |> Lexer.Token.is_raw "from_scratch"
      then (
        Lexer.advance lexer;
        Lexer.expect_next lexer ";";
        go acc)
      else (
        let rule = Rule.parse lexer in
        Log.trace (fun log ->
          log "Parsed rule %a at %a" String.print_debug rule.name Span.print rule.span);
        Lexer.expect_next lexer ";";
        go (add rule acc)))
    else (
      Error.error peek.span "Unexpected %a" Lexer.Token.print peek;
      failwith "NOT CONTINUING")
  in
  go empty
;;

let parse_list : string list -> t =
  fun rules ->
  rules
  |> List.map (fun line ->
    let lexer =
      Lexer.init
        Lexer.default_rules
        { contents = line; uri = Uri.of_string "ocaml:parse_list/rule" }
    in
    let rule = Rule.parse lexer in
    Lexer.expect_eof lexer;
    rule)
  |> of_list
;;

let find_category_opt category ruleset =
  ruleset.categories |> CategoryMap.find_opt category
;;

let find_rule_opt category name ruleset =
  ruleset
  |> find_category_opt category
  |> Option.and_then (fun category -> category |> Category.find_rule_opt name)
;;
