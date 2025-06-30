open Std

module Var : sig
  type 'a t
  type 'a var = 'a t

  val new_not_inferred : 'a. unit -> 'a var
  val new_inferred : 'a. 'a -> 'a var
  val inferred_opt : 'a. 'a var -> 'a option
  val print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit
  val unite : 'a. ('a -> 'a -> 'a) -> 'a var -> 'a var -> 'a var
  val infer_as : 'a. ('a -> 'a -> 'a) -> 'a -> 'a var -> unit
end

type 'a var = 'a Var.t
