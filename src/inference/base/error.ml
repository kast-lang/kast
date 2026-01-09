open Std
open Kast_util

type error =
  { msg : formatter -> unit
  ; span : span
  }

type _ Effect.t += Error : error -> unit Effect.t

let error : 'a. span -> ('a, formatter, unit, unit) format4 -> 'a =
  fun span format ->
  Format.kdprintf (fun msg -> Effect.perform <| Error { msg; span }) format
;;

let print : formatter -> error -> unit =
  fun fmt error ->
  fprintf
    fmt
    "@{<red>Inference error:@} %t @{<dim>at %a@}"
    error.msg
    Span.print
    error.span
;;

let error_context : 'a. (unit -> 'a) -> (formatter -> unit) -> 'a =
  fun f print_ctx ->
  try f () with
  | effect Error e, k ->
    Effect.perform
    <| Error
         { msg =
             (fun fmt ->
               e.msg fmt;
               fprintf fmt " \n";
               print_ctx fmt)
         ; span = e.span
         };
    Effect.continue k ()
;;
