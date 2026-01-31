open Std
open Kast_util

module Category = struct
  type t =
    | Global
    | DomainSpecific of { name : string }
  [@@deriving eq, ord]

  let print fmt = function
    | Global -> fprintf fmt "Global"
    | DomainSpecific { name } -> fprintf fmt "Domain %a" String.print_debug name
  ;;
end

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
      | Filter of filter

    let compare = Float.compare

    let stricter_filter (a : filter) (b : filter) =
      match a, b with
      | Any, p | p, Any -> p
      | Greater a, Greater b -> Greater (Float.max a b)
      | GreaterOrEqual a, GreaterOrEqual b -> GreaterOrEqual (Float.max a b)
      | Greater a, GreaterOrEqual b | GreaterOrEqual b, Greater a ->
        if a >= b then Greater a else GreaterOrEqual b
    ;;

    let make_filter : filter_kind -> priority -> filter =
      fun kind p ->
      match kind with
      | Greater -> Greater p
      | GreaterOrEqual -> GreaterOrEqual p
      | Filter f -> f
    ;;

    let check_filter_with_range (range : priority Range.Inclusive.t) (filter : filter) =
      match filter with
      | Any -> true
      | Greater x -> range.max > x
      | GreaterOrEqual x -> range.max >= x
    ;;

    let check_filter (p : priority) (filter : filter) =
      match filter with
      | Any -> true
      | Greater x -> p > x
      | GreaterOrEqual x -> p >= x
    ;;
  end

  type priority = Priority.t

  module WrapMode = struct
    type t =
      | Never
      | Always
      | IfAnyAssociative
      | IfAnyNonAssociative
  end

  type wrap_mode = WrapMode.t
  type quantifier = Optional

  type binding =
    { name : string option
    ; priority : Priority.filter_kind
    ; category : Category.t
    }

  type part =
    | Whitespace of
        { nowrap : string
        ; wrap : string
        }
    | Keyword of string
    | Value of binding
    | Group of group

  and group =
    { id : Id.t
    ; nested : group_nested
    ; wrap_mode : wrap_mode option
    ; parts : part list
    ; quantifier : quantifier option
    }

  and group_nested =
    | Flat
    | Nested of { name : string option }

  let equal_group a b = a.id = b.id
  let compare_group a b = Id.compare a.id b.id

  type t =
    { id : Id.t
    ; span : span
    ; category : Category.t
    ; name : string
    ; priority : float
    ; parts : part list
    ; do_parse : bool
    ; wrap_mode : wrap_mode
    }

  let equal a b = a.id = b.id
  let compare a b = Id.compare a.id b.id

  type rule = t

  let print : formatter -> rule -> unit =
    fun fmt rule ->
    (match rule.category with
     | Global -> ()
     | DomainSpecific { name } -> fprintf fmt "%a." String.print_debug name);
    fprintf fmt "%a" String.print_debug rule.name
  ;;

  let keywords : rule -> string Seq.t =
    fun rule ->
    List.to_seq rule.parts
    |> Seq.filter_map (function
      | Keyword keyword -> Some keyword
      | _ -> None)
  ;;

  module Part = struct
    type t = part

    let print : formatter -> part -> unit =
      fun fmt -> function
      | Whitespace _ -> fprintf fmt "whitespace"
      | Keyword keyword -> fprintf fmt "keyword %a" String.print_debug keyword
      | Value binding ->
        fprintf fmt "value %a" (Option.print String.print_debug) binding.name
      | Group group ->
        (match group.nested with
         | Flat -> fprintf fmt "group <flat>"
         | Nested { name } ->
           fprintf fmt "group %a" (Option.print String.print_debug) name)
    ;;
  end
end

type rule = Rule.t
