open Std

type t = unit
type eval_fn = { f : 'a. 'a Expr.t -> (t -> 'a) option }

let init () : t = ()
let eval_impls : eval_fn list Atomic.t = Atomic.make []

let register_eval : eval_fn -> unit =
 fun f -> Atomic.set eval_impls (f :: Atomic.get eval_impls)

let eval : 'a. 'a Expr.t -> t -> 'a =
 fun expr state ->
  let f =
    Atomic.get eval_impls
    |> List.find_map (fun f -> f.f expr)
    |> Option.unwrap_or_else (fun () -> failwith __LOC__)
  in
  f state
