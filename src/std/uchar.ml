open Char
open Format

module Uchar = struct
  include Stdlib.Uchar

  let print : formatter -> t -> unit =
    (* TODO global buffer would be subject to races *)
    (* let buffer = Bytes.create (Uchar.max_utf_8_decode_length) in *)
    fun fmt c ->
    let len = Stdlib.Uchar.utf_8_byte_length c in
    let buffer = Bytes.create len in
    if Bytes.set_utf_8_uchar buffer 0 c <> len then failwith "how???";
    buffer |> Bytes.iter (fun c -> Char.print fmt c)
  ;;

  let needs_escaping : in_string:bool -> t -> bool =
    fun ~in_string c ->
    if Stdlib.Uchar.is_char c
    then (
      match Stdlib.Uchar.to_char c with
      | '\\' -> true
      | '\n' -> true
      | '\t' -> true
      | '\r' -> true
      | '\b' -> true
      | '\'' -> not in_string
      | '"' -> in_string
      | _ -> false)
    else false
  ;;

  let print_maybe_escaped : in_string:bool -> formatter -> t -> unit =
    fun ~in_string fmt c ->
    if Stdlib.Uchar.is_char c
    then (
      match Stdlib.Uchar.to_char c with
      | '\\' -> fprintf fmt "\\\\"
      | '\n' -> fprintf fmt "\\n"
      | '\t' -> fprintf fmt "\\t"
      | '\r' -> fprintf fmt "\\r"
      | '\b' -> fprintf fmt "\\b"
      | '\'' when not in_string -> fprintf fmt "\\'"
      | '"' when in_string -> fprintf fmt "\\\""
      | _ -> print fmt c)
    else print fmt c
  ;;

  let print_debug : formatter -> t -> unit =
    fun fmt c -> fprintf fmt "'%a'" (print_maybe_escaped ~in_string:false) c
  ;;
end
