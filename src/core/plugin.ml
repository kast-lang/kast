open Std

module TyP = struct
  module type S = sig
    type t
    type Ty.t += T of t

    val print : formatter -> t -> unit
  end

  let register : (module S) -> unit =
   fun (module P) ->
    Ty.register_print (function
      | P.T value -> Some (fun fmt -> P.print fmt value)
      | _ -> None)
end

module ValueP = struct
  module type S = sig
    type t
    type Value.t += T of t

    val print : formatter -> t -> unit
    val typeof : t -> Ty.t
  end

  let register : (module S) -> unit =
   fun (module P) ->
    Value.register_print (function
      | P.T value -> Some (fun fmt -> P.print fmt value)
      | _ -> None);
    Value.register_typeof (function
      | P.T value -> Some (P.typeof value)
      | _ -> None)
end

module ExprP = struct
  module type S = sig
    type t
    type result
    type _ Expr.t += T : t -> result Expr.t

    val eval : t -> Interpreter.t -> result
  end

  let register : (module S) -> unit =
   fun (module P) ->
    Interpreter.register_eval
      {
        f =
          (fun (type a) (value : a Expr.t) : (Interpreter.t -> a) option ->
            match value with
            | P.T value -> Some (fun i -> P.eval value i)
            | _ -> None);
      }
end

module Ty = TyP
module Value = ValueP
module Expr = ExprP
