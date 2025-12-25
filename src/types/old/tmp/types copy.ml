open Std
open Kast_util
module Ast = Kast_ast
module Syntax = Kast_syntax
module Inference = Kast_inference_base
module Label = Label

  (* blocked_value *)

  (* EXPR *)

  (* PLACE EXPR *)


  (* ASSIGNEE EXPR *)

  (* TYPE EXPR *)

  (* PATTERN *)

  (* SCOPE *)

  (* interpreter *)

  (* OTHER *)



end

and ValueVar : Inference.Var.S = Inference.Var.Make (ValueShape)
and ValueShape : sig end = struct end
and Value : sig end = struct end

and ValueImpl : sig
  type t = TypesImpl.value

  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type t = TypesImpl.value

  module RecurseCache = Inference.CompareRecurseCache

  let equal a b =
    RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
        TypesImpl.equal_value a b)

  let compare a b =
    RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
        TypesImpl.compare_value a b)
end


let target_symbol : symbol = Symbol.create "target"

include TypesImpl
