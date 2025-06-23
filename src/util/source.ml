open Std

module Position = struct
  type t = {
    index : int;
    line : int;
    column : int;
  }

  type position = t

  let beginning : position = { index = 0; line = 1; column = 1 }

  let advance : char -> position -> position =
   fun c p ->
    match c with
    | '\n' -> { index = p.index + 1; line = p.line + 1; column = 1 }
    | _ -> { index = p.index + 1; line = p.line; column = p.column + 1 }

  let print : 'a. formatter -> position -> unit =
   fun fmt { index = _; line; column } -> fprintf fmt "%d:%d" line column
end

type position = Position.t

module Source = struct
  type t = {
    contents : string;
    filename : string;
  }
end

type source = Source.t

module Span = struct
  type t = {
    start : position;
    finish : position;
    filename : string;
  }

  type span = t

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; filename } ->
    fprintf fmt "%s:%d.%d-%d.%d" filename start.line start.column finish.line
      finish.column
end

type span = Span.t

module Spanned = struct
  type 'a t = {
    value : 'a;
    span : span;
  }

  type 'a spanned = 'a t

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a spanned -> unit =
   fun print_value fmt spanned ->
    fprintf fmt "%a at %a" print_value spanned.value Span.print spanned.span
end

type 'a spanned = 'a Spanned.t
