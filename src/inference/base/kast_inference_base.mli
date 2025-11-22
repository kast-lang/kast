open Std
open Kast_util

type 'a unite = span:span -> 'a -> 'a -> 'a

module Var : sig
  type 'a t
  type 'a var = 'a t

  val new_not_inferred : 'a. unit -> 'a var
  val new_inferred : 'a. 'a -> 'a var
  val inferred_opt : 'a. 'a var -> 'a option
  val print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit
  val unite : 'a. 'a unite -> 'a var unite
  val infer_as : 'a. 'a unite -> span:span -> 'a -> 'a var -> unit
  val once_inferred : 'a. ('a -> unit) -> 'a var -> unit
  val await_inferred : 'a. 'a var -> 'a
  val same : 'a. 'a var -> 'a var -> bool
  val recurse_id : 'a. 'a var -> id
end

val fork : (unit -> unit) -> unit

type 'a var = 'a Var.t

module Error = Error
