open Std
open Kast_util

type t =
  { msg : formatter -> unit
  ; span : span
  }

type error = t
type _ Effect.t += Error : error -> unit Effect.t

let print : formatter -> error -> unit =
  fun fmt error ->
  fprintf fmt "@{<red>Parse error:@} %t @{<dim>at %a@}" error.msg Span.print error.span
;;

let error : span -> ('a, formatter, unit, unit) format4 -> 'a =
  fun span format ->
  Format.kdprintf (fun msg -> Effect.perform <| Error { msg; span }) format
;;
