open Std

type t = unit

let default () : t = ()

module AbstractEval = struct
  module type E = sig
    type t
    type result
  end

  module Make (E : E) = struct
    type eval_fn = E.t -> (t -> E.result) option

    let eval_impls : eval_fn list Atomic.t = Atomic.make []

    let register : eval_fn -> unit =
     fun f -> Atomic.set eval_impls (f :: Atomic.get eval_impls)

    let eval : E.t -> t -> E.result =
     fun expr state ->
      let f =
        Atomic.get eval_impls
        |> List.find_map (fun f -> f expr)
        |> Option.unwrap_or_else (fun () -> failwith __LOC__)
      in
      f state
  end
end

module EvalExpr = AbstractEval.Make (Expr)
module EvalTy = AbstractEval.Make (Expr.Ty)

module Eval = struct
  module Expr = EvalExpr

  let expr = Expr.eval

  module Ty = EvalTy

  let ty = Ty.eval
end
