open Std
open Kast_util

type error = {
  msg : formatter -> unit;
  span : span;
}

exception Error of error

let () =
  Printexc.register_printer (function
    | Error error ->
        eprintln "%a" (fun fmt () -> error.msg fmt) ();
        exit 1
    | _ -> None)

let error : 'never. span -> ('a, formatter, unit, 'never) format4 -> 'a =
 fun span format ->
  Format.kdprintf (fun msg -> raise <| Error { msg; span }) format
