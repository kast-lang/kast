open Std

module Position = struct
  type t = { index : int; line : int; column : int }
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
  type t = { contents : string; filename : string }
end

type source = Source.t

module Span = struct
  type t = { start : position; finish : position; filename : string }
  type span = t

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; filename } ->
    fprintf fmt "%s:%d.%d-%d.%d" filename start.line start.column finish.line
      finish.column
end

type span = Span.t

module Spanned = struct
  type 'a t = { value : 'a; span : span }
  type 'a spanned = 'a t

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a spanned -> unit =
   fun print_value fmt spanned ->
    fprintf fmt "%a at %a" print_value spanned.value Span.print spanned.span
end

type 'a spanned = 'a Spanned.t

module Tuple = struct
  module StringMap = Map.Make (String)

  type 'a t = { unnamed : 'a array; named : 'a StringMap.t }

  let empty : 'a. 'a t = { unnamed = [||]; named = StringMap.empty }

  let add : 'a. string option -> 'a -> 'a t -> 'a t =
   fun (type a) (name : string option) (value : a) (tuple : a t) : a t ->
    match name with
    | Some name -> { tuple with named = StringMap.add name value tuple.named }
    | None ->
        { tuple with unnamed = Array.append tuple.unnamed (Array.make 1 value) }

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a t -> unit =
   fun print_value fmt { unnamed; named } ->
    fprintf fmt "{";
    Format.pp_print_iter ~pp_sep:Format.comma_separator Array.iter print_value
      fmt unnamed;
    if StringMap.cardinal named <> 0 then Format.comma_separator fmt ();
    let print_named fmt (name, value) =
      fprintf fmt "%S = %a" name print_value value
    in
    Format.pp_print_iter ~pp_sep:Format.comma_separator
      (fun f -> StringMap.iter (fun name value -> f (name, value)))
      print_named fmt named;
    fprintf fmt "}"
end

type 'a tuple = 'a Tuple.t
