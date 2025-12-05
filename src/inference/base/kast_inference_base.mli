open Std
open Kast_util

type 'a unite = span:span -> 'a -> 'a -> 'a

module Var : sig
  type 'a t
  type 'a var = 'a t
  type _ Effect.t += AwaitUpdate : 'a. 'a var -> bool Effect.t

  val new_not_inferred : 'a. span:span -> 'a var
  val new_inferred : 'a. span:span -> 'a -> 'a var
  val inferred_opt : 'a. 'a var -> 'a option
  val print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit
  val unite : 'a. 'a unite -> 'a var unite
  val infer_as : 'a. 'a unite -> span:span -> 'a -> 'a var -> unit
  val once_inferred : 'a. ('a -> unit) -> 'a var -> unit
  val await_inferred : 'a. error_shape:'a -> 'a var -> 'a
  val recurse_id : 'a. 'a var -> id
  val spans : 'a. 'a var -> Set.Make(Span).t

  module Map : sig
    type 'a t

    val create : 'a. unit -> 'a t
    val add : 'a 'v. 'v var -> 'a -> 'a t -> unit
    val find_opt : 'a 'v. 'v var -> 'a t -> 'a option
  end
end

val fork : (unit -> unit) -> unit

type 'a var = 'a Var.t

val equal_var : 'a. ('a -> 'a -> bool) -> 'a var -> 'a var -> bool
val compare_var : 'a. ('a -> 'a -> int) -> 'a var -> 'a var -> int

module Error = Error
