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
