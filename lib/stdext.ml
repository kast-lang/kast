module Format = struct
  include Format

  let comma_separator : formatter -> unit -> unit =
   fun fmt () ->
    pp_print_custom_break fmt ~fits:(",", 1, "") ~breaks:(",", 0, "")

  let trailing_comma : formatter -> unit -> unit =
   fun fmt () -> pp_print_custom_break fmt ~fits:("", 0, "") ~breaks:(",", 0, "")
end

type formatter = Format.formatter

let fprintf = Format.fprintf

let eprintln : 'a. ('a, formatter, unit) format -> 'a =
 fun format ->
  Format.kfprintf (fun fmt -> fprintf fmt "\n") Format.err_formatter format

module Char = struct
  include Char

  let is_alpha : char -> bool = function
    | 'A' .. 'Z' -> true
    | 'a' .. 'z' -> true
    | _ -> false

  let is_digit : char -> bool = function '0' .. '9' -> true | _ -> false
  let is_alphanumberic : char -> bool = fun c -> is_alpha c || is_digit c

  let is_whitespace : char -> bool =
   fun c -> c == ' ' || c == '\t' || c == '\r' || c == '\n'

  let print : formatter -> char -> unit = fun fmt c -> fprintf fmt "%c" c
end

module String = struct
  include String

  let get : string -> int -> char option =
   fun s i ->
    if 0 <= i && i < String.length s then Some (String.get s i) else None
end

module Option = struct
  include Option

  let map_or : 'a 'r. 'r -> ('a -> 'r) -> 'a option -> 'r =
   fun default f opt -> match opt with Some x -> f x | None -> default

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a option -> unit =
   fun print_value fmt opt ->
    match opt with
    | None -> fprintf fmt "None"
    | Some value -> print_value fmt value
end

let ( let* ) = Option.bind

module List = struct
  include List

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a list -> unit =
   fun print_value fmt list ->
    fprintf fmt "[";
    Format.pp_print_iter ~pp_sep:Format.comma_separator List.iter print_value
      fmt list;
    Format.trailing_comma fmt ();
    fprintf fmt "]"
end
