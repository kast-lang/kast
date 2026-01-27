open Uchar
open Format
open Option
open Char

module String = struct
  include Stdlib.String

  let get : string -> int -> char option =
    fun s i -> if 0 <= i && i < length s then Some (Stdlib.String.get s i) else None
  ;;

  let iteri_utf8 : (int -> Uchar.t -> unit) -> string -> unit =
    fun f s ->
    let i = ref 0 in
    let len = Stdlib.String.length s in
    while !i < len do
      let c = Stdlib.String.get_utf_8_uchar s !i in
      let new_i = !i + Uchar.utf_decode_length c in
      let c = Uchar.utf_decode_uchar c in
      f !i c;
      i := new_i
    done
  ;;

  let iter_utf8 : (Uchar.t -> unit) -> string -> unit =
    fun f s -> s |> iteri_utf8 (fun _i c -> f c)
  ;;

  let into_single_utf8 : string -> Uchar.t option =
    fun s ->
    let c = Stdlib.String.get_utf_8_uchar s 0 in
    if Uchar.utf_decode_length c = length s then Some (Uchar.utf_decode_uchar c) else None
  ;;

  let from_single_utf8 : Uchar.t -> string =
    fun c ->
    let len = Uchar.utf_8_byte_length c in
    let buffer = Bytes.create len in
    if Bytes.set_utf_8_uchar buffer 0 c <> len then failwith "how???";
    of_bytes buffer
  ;;

  let print : formatter -> string -> unit = fun fmt s -> fprintf fmt "%s" s

  let print_debug : formatter -> string -> unit =
    fun fmt s ->
    fprintf fmt "\"";
    s |> iter_utf8 (fun c -> Uchar.print_maybe_escaped ~in_string:true fmt c);
    fprintf fmt "\""
  ;;

  let print_maybe_escaped : formatter -> string -> unit =
    fun fmt s ->
    let needs_escaping = ref false in
    s
    |> iter_utf8 (fun c ->
      if Uchar.needs_escaping ~in_string:true c then needs_escaping := true);
    if !needs_escaping then print_debug fmt s else print fmt s
  ;;

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
