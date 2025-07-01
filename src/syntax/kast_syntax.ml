open Std
open Kast_util

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
  type quantifier = Optional

  type binding = {
    name : string option;
    priority : Priority.filter_kind;
  }

  type part =
    | Whitespace of {
        nowrap : string;
        wrap : string;
      }
    | Keyword of string
    | Value of binding
    | Group of group

  and group = {
    name : string option;
    parts : part list;
    quantifier : quantifier option;
  }

  module WrapMode = struct
    type t =
      | Never
      | Always
      | IfAny
  end

  type wrap_mode = WrapMode.t

  type rule = {
    span : span;
    name : string;
    priority : float;
    parts : part list;
    wrap_mode : wrap_mode;
  }

  type t = rule

  let print : formatter -> rule -> unit =
   fun fmt rule -> fprintf fmt "%S" rule.name

  let keywords : rule -> string Seq.t =
   fun rule ->
    List.to_seq rule.parts
    |> Seq.filter_map (function
         | Keyword keyword -> Some keyword
         | _ -> None)

  module Part = struct
    type t = part

    let print : formatter -> part -> unit =
     fun fmt -> function
      | Whitespace _ -> fprintf fmt "whitespace"
      | Keyword keyword -> fprintf fmt "keyword %S" keyword
      | Value binding ->
          fprintf fmt "value %a" (Option.print String.print_dbg) binding.name
      | Group group ->
          fprintf fmt "group %a" (Option.print String.print_dbg) group.name
  end
end

type rule = Rule.t
