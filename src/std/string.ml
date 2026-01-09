open Char
open Format
open Option

module String = struct
  include Stdlib.String

  let get : string -> int -> char option =
    fun s i -> if 0 <= i && i < length s then Some (Stdlib.String.get s i) else None
  ;;

  let print_maybe_escaped : formatter -> string -> unit =
    fun fmt s ->
    if
      for_all
        (function
          | 'a' .. 'z' -> true
          | 'A' .. 'Z' -> true
          | '0' .. '9' -> true
          | c when contains "|-/_.:" c -> true
          | _ -> false)
        s
    then fprintf fmt "%s" s
    else fprintf fmt "@{<dim>\"@}%s@{<dim>\"@}" (escaped s)
  ;;

  let print : formatter -> string -> unit = fun fmt s -> fprintf fmt "%s" s
  let print_dbg : formatter -> string -> unit = fun fmt s -> fprintf fmt "%S" s
  let is_whitespace : string -> bool = for_all Char.is_whitespace

  let strip_prefix ~prefix s =
    if starts_with ~prefix s
    then Some (sub s (length prefix) (length s - length prefix))
    else None
  ;;

  let strip_suffix ~suffix s =
    if ends_with ~suffix s then Some (sub s 0 (length s - length suffix)) else None
  ;;

  let strip ~prefix ~suffix s =
    s |> strip_prefix ~prefix |> Option.and_then (strip_suffix ~suffix)
  ;;
end
