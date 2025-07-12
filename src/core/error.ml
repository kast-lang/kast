open Std
open Kast_util

type t = {
  msg : formatter -> unit;
  span : span;
}

type _ Effect.t += T : t -> unit Effect.t

let throw : 'a. span -> ('a, formatter, unit, unit) format4 -> 'a =
 fun span format ->
  Format.kdprintf (fun msg -> Effect.perform <| T { msg; span }) format

let print : formatter -> t -> unit =
 fun fmt error ->
  fprintf fmt "@{<red>Error:@} %t @{<dim>at %a@}" error.msg Span.print
    error.span
