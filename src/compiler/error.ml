open Std
open Kast_util

type error = {
  msg : formatter -> unit;
  span : span;
}

exception Error of error

let () =
  Printexc.register_printer (function
    | Error { msg; span } ->
        eprintln "%a at %a" (fun fmt () -> msg fmt) () Span.print span;
        exit 1
    | _ -> None)

let error : 'a 'never. span -> ('a, formatter, unit, 'never) format4 -> 'a =
 fun span format ->
  Format.kdprintf (fun msg -> raise <| Error { msg; span }) format
