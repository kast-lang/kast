open Std
open Kast_util

module TyP = struct
  module type S = sig
    type t
    type Ty.Shape.t += T of t

    val print : formatter -> t -> unit
  end

  let register : (module S) -> unit =
   fun (module P) ->
    Ty.Shape.register_print (function
      | P.T value -> Some (fun fmt -> P.print fmt value)
      | _ -> None)
end

module ValueP = struct
  module type S = sig
    type t
    type Value.Shape.t += T of t

    val print : formatter -> t -> unit
    val typeof : t -> Ty.t
  end

  let register : (module S) -> unit =
   fun (module P) ->
    Value.Shape.register_print (function
      | P.T value -> Some (fun fmt -> P.print fmt value)
      | _ -> None);
    Value.Shape.register_typeof (function
      | P.T value -> Some (P.typeof value)
      | _ -> None)
end

module ExprP = struct
  module type E = sig
    type result

    module Shape : sig
      type t = ..
    end

    type t = {
      shape : Shape.t;
      span : span;
      ty : Ty.t;
    }

    module Eval : sig
      val register : (t -> (Interpreter.t -> result) option) -> unit
    end
  end

  module Make (E : E) = struct
    module type S = sig
      type t
      type E.Shape.t += T of t

      val eval : span -> t -> Interpreter.t -> E.result
    end

    let register : (module S) -> unit =
     fun (module P) ->
      E.Eval.register (fun (expr : E.t) : (Interpreter.t -> E.result) option ->
          match expr.shape with
          | P.T value -> Some (fun i -> P.eval expr.span value i)
          | _ -> None)
  end

  include Make (struct
    include Expr
    module Eval = Interpreter.Eval.Expr
  end)

  module Ty = Make (struct
    include Expr.Ty
    module Eval = Interpreter.Eval.Ty
  end)
end

module Ty = TyP
module Value = ValueP
module Expr = ExprP
