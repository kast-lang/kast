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
        eprintln "@{<red>Error:@} %a" (fun fmt () -> f fmt) ();
        exit 1
    | Failure s ->
        eprintln "@{<red>Error:@} %s" s;
        exit 1
    | _ -> None)

let fail f = Format.kdprintf (fun f -> raise @@ FailFormat f) f
