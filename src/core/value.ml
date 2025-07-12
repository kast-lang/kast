open Std

module T = struct
  type t = ..
end

include T
include Print.Make (T)

type typeof_fn = t -> Ty.t option

let typeof_impls : typeof_fn list Atomic.t = Atomic.make []

let register_typeof : typeof_fn -> unit =
 fun f -> Atomic.set typeof_impls (f :: Atomic.get typeof_impls)

let typeof : t -> Ty.t =
 fun value ->
  Atomic.get typeof_impls
  |> List.find_map (fun f -> f value)
  |> Option.unwrap_or_else (fun () -> failwith __LOC__)
