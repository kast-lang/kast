module Position = struct
  type t = { index : int; line : int; column : int }
  type position = t

  let beginning : position = { index = 0; line = 1; column = 1 }

  let advance : char -> position -> position =
   fun c p ->
    match c with
    | '\n' -> { index = p.index + 1; line = p.line + 1; column = 1 }
    | _ -> { index = p.index + 1; line = p.line; column = p.column + 1 }

  let show : position -> string =
   fun { index = _; line; column } ->
    Int.to_string line ^ ":" ^ Int.to_string column
end

type position = Position.t

module Source = struct
  type t = { contents : string; filename : string }
end

type source = Source.t

module Span = struct
  type t = { start : position; finish : position; filename : string }
  type span = t

  let show : span -> string =
   fun { start; finish; filename } ->
    filename ^ ":" ^ Position.show start ^ " ~ " ^ Position.show finish
end

type span = Span.t

module Spanned = struct
  type 'a t = { value : 'a; span : span }
  type 'a spanned = 'a t

  let show : 'a. ('a -> string) -> 'a spanned -> string =
   fun show_a spanned -> show_a spanned.value ^ " at " ^ Span.show spanned.span
end

type 'a spanned = 'a Spanned.t

(* :) *)
type token = Token.t
