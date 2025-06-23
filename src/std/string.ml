open Char
open Format

module String = struct
  include Stdlib.String

  let get : string -> int -> char option =
   fun s i ->
    if 0 <= i && i < length s then Some (Stdlib.String.get s i) else None

  let print_dbg : formatter -> string -> unit = fun fmt s -> fprintf fmt "%S" s
  let is_whitespace : string -> bool = for_all Char.is_whitespace
end
