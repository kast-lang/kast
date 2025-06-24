open Format

module Char = struct
  include Stdlib.Char

  let is_alpha : char -> bool = function
    | 'A' .. 'Z' -> true
    | 'a' .. 'z' -> true
    | _ -> false

  let is_digit : char -> bool = function
    | '0' .. '9' -> true
    | _ -> false

  let is_alphanumeric : char -> bool = fun c -> is_alpha c || is_digit c

  let is_whitespace : char -> bool =
   fun c -> c = ' ' || c = '\t' || c = '\r' || c = '\n'

  let print : formatter -> char -> unit = fun fmt c -> fprintf fmt "%c" c
end
