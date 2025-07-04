include Format
include Char
include String
include Option
include List
include Array
include Return
include Ord
include Range
include Log
include Bool
include Set
include Int

let unreachable format =
  Format.fprintf Format.str_formatter "unreachable reached: ";
  Format.kfprintf
    (fun _fmt ->
      let msg = Format.flush_str_formatter () in
      failwith msg)
    Format.str_formatter format

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

exception FailFormat of (formatter -> unit)

let () =
  Printexc.register_printer (function
    | FailFormat f ->
        f Format.str_formatter;
        Some (Format.flush_str_formatter ())
    | _ -> None)

let ( <| ) = ( @@ )
let fail f = Format.kdprintf (fun f -> raise <| FailFormat f) f

let read_from_filesystem path =
  let ch = In_channel.open_text path in
  Fun.protect
    (fun () -> In_channel.input_all ch)
    ~finally:(fun () -> In_channel.close ch)
