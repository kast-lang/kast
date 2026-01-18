open Std
open Kast_util
module Token = Kast_token
module Syntax = Kast_syntax

type simple =
  { comments_before : Token.comment list
  ; token : Token.t
  }
[@@deriving eq, ord]

module SyntaxMode = struct
  type t =
    | Define of Syntax.Rule.t
    | FromScratch
  [@@deriving eq, ord]

  let print fmt mode =
    fprintf fmt "@{<yellow>";
    (match mode with
     | Define rule -> fprintf fmt "@syntax %S" rule.name
     | FromScratch -> fprintf fmt "@syntax from_scratch");
    fprintf fmt "@}"
  ;;
end

module type DataS = sig
  type t

  val span : t -> span
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type S = sig
  module Data : DataS
  module SyntaxMode = SyntaxMode

  type part =
    | Comment of Token.comment
    | Value of ast
    | Keyword of Token.t
    | Group of group

  and complex =
    { rule : Syntax.Rule.t
    ; root : group
    }

  and group =
    { (* TODO rule is only None for root *)
      rule : Syntax.Rule.group option
    ; parts : part list
    ; children : child Tuple.t
    ; span : Span.t
    }

  and child =
    | Ast of ast
    | Group of group

  and syntax =
    { comments_before : Token.comment list
    ; mode : SyntaxMode.t
    ; tokens : Token.t list (* TODO more typed parts so we can highlight better? *)
    ; value_after : ast option
    }

  and error = { parts : part list }

  and shape =
    | Empty
    | Simple of simple
    | Complex of complex
    | Syntax of syntax
    | Error of error

  and t =
    { shape : shape
    ; data : Data.t
    }

  and ast = t [@@deriving eq, ord]

  val print : formatter -> t -> unit

  module Shape : sig
    type t = shape

    val print : formatter -> t -> unit
    val print_short : formatter -> t -> unit
  end

  module Child : sig
    type t = child

    val expect_ast : child -> ast
    val expect_group : child -> group
    val print : formatter -> t -> unit
  end

  module Part : sig
    type t = part

    val print : formatter -> t -> unit
  end

  val flatten_children : child tuple -> ast tuple

  val collect_list
    :  binary_rule_name:string
    -> ?trailing_or_leading_rule_name:string
    -> ast
    -> ast list
end

module Make (Data : DataS) : S with module Data = Data = struct
  module Data = Data
  module SyntaxMode = SyntaxMode

  type part =
    | Comment of Token.comment
    | Value of ast
    | Keyword of Token.t
    | Group of group

  and complex =
    { rule : Syntax.Rule.t
    ; root : group
    }

  and group =
    { (* TODO rule is only None for root *)
      rule : Syntax.Rule.group option
    ; parts : part list
    ; children : child Tuple.t
    ; span : Span.t
    }

  and child =
    | Ast of ast
    | Group of group

  and syntax =
    { comments_before : Token.comment list
    ; mode : SyntaxMode.t
    ; tokens : Token.t list (* TODO more typed parts so we can highlight better? *)
    ; value_after : ast option
    }

  and error = { parts : part list }

  and shape =
    | Empty
    | Simple of simple
    | Complex of complex
    | Syntax of syntax
    | Error of error

  and t =
    { shape : shape
    ; data : Data.t
    }

  and ast = t [@@deriving eq, ord]

  let rec print : formatter -> ast -> unit =
    fun fmt { shape; data } ->
    let span = Data.span data in
    fprintf fmt "%a @{<dim>at %a@}" print_shape shape Span.print span

  and print_part =
    fun fmt -> function
    | Comment _ -> fprintf fmt "<comment>"
    | Keyword token -> fprintf fmt "keyword %a" Token.print token
    | Value value -> print fmt value
    | Group group -> print_group fmt group

  and print_child : formatter -> child -> unit =
    fun fmt -> function
    | Ast ast -> print fmt ast
    | Group group -> print_group fmt group

  and print_group : formatter -> group -> unit =
    fun fmt { rule = _; parts = _; span = _; children } ->
    Tuple.print print_child fmt children

  and print_shape : formatter -> shape -> unit =
    fun fmt -> function
    | Empty -> fprintf fmt "@{<magenta><empty>@}"
    | Simple { comments_before = _; token } -> Token.Shape.print fmt token.shape
    | Complex { rule; root } ->
      fprintf
        fmt
        "@{<magenta>%a@} %a"
        String.print_maybe_escaped
        rule.name
        print_group
        root
    | Syntax { comments_before = _; mode; value_after; tokens = _ } ->
      SyntaxMode.print fmt mode;
      (match value_after with
       | None -> ()
       | Some value -> fprintf fmt "\n%a" print value)
    | Error { parts } -> fprintf fmt "@{<red><error> %a@}" (List.print print_part) parts

  and print_shape_short : formatter -> shape -> unit =
    fun fmt -> function
    | Empty -> fprintf fmt "@{<magenta><empty>@}"
    | Simple { comments_before = _; token } -> Token.Shape.print fmt token.shape
    | Complex { rule; root = _ } ->
      fprintf fmt "@{<magenta>%a@}" String.print_maybe_escaped rule.name
    | Syntax { comments_before = _; mode = _; value_after = _; tokens = _ } ->
      fprintf fmt "<syntax>"
    | Error _ -> fprintf fmt "@{<red><error>@}"
  ;;

  module Shape = struct
    type t = shape

    let print = print_shape
    let print_short = print_shape_short
  end

  module Child = struct
    type t = child

    let expect_ast = function
      | Ast ast -> ast
      | Group _ -> fail "expected ast, got group"
    ;;

    let expect_group = function
      | Ast _ -> fail "expected group, got ast"
      | Group group -> group
    ;;

    let print = print_child
  end

  module Part = struct
    type t = part

    let print = print_part
  end

  let rec collect_list
            ~(binary_rule_name : string)
            ?(trailing_or_leading_rule_name : string option)
            (ast : ast)
    : ast list
    =
    match ast.shape with
    | Complex { rule = { name; _ }; root } when Some name = trailing_or_leading_rule_name
      ->
      collect_list
        ~binary_rule_name
        (root.children |> Tuple.unwrap_single_unnamed |> Child.expect_ast)
    | Complex { rule = { name; _ }; root } when name = binary_rule_name ->
      let a, b = root.children |> Tuple.map Child.expect_ast |> Tuple.unwrap_unnamed2 in
      let a = collect_list ~binary_rule_name a in
      let b = collect_list ~binary_rule_name b in
      a @ b
    | _ -> [ ast ]
  ;;

  let rec flatten_children (children : child tuple) : ast tuple =
    let result = ref Tuple.empty in
    children
    |> Tuple.iter (fun member child ->
      let member =
        match member with
        | Index _ -> None
        | Name name -> Some name
      in
      match (child : child) with
      | Ast ast -> result := !result |> Tuple.add member ast
      | Group group -> result := Tuple.merge !result (group.children |> flatten_children));
    !result
  ;;
end

module T = Make (struct
    type t = span

    let span span = span
    let equal = Span.equal
    let compare = Span.compare
  end)
