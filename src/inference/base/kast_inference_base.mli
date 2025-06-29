open Std

module Var : sig
  type 'a t
  type 'a var = 'a t

  val new_not_inferred : 'a. unit -> 'a var
  val new_inferred : 'a. 'a -> 'a var
  val print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit
  val unite : 'a. ('a -> 'a -> 'a) -> 'a var -> 'a var -> 'a var
end

type 'a var = 'a Var.t
