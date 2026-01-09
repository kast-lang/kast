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
  fprintf fmt "@{<red>Compiler error:@} %t @{<dim>at %a@}" error.msg Span.print error.span
;;
