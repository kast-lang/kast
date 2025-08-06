open Std

module Shape = struct
  module T = struct
    type t = ..
  end

  include T
  include Print.Make (T)

  module Error = struct
    type T.t += T : T.t
  end

  let init () =
    register_print (fun expr ->
        match expr with
        | Error.T -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
        | _ -> None)

  type typeof_fn = t -> Ty.t option

  let typeof_impls : typeof_fn list Atomic.t = Atomic.make []

  let register_typeof : typeof_fn -> unit =
   fun f -> Atomic.set typeof_impls (f :: Atomic.get typeof_impls)

  let typeof : t -> Ty.t =
   fun value ->
    Atomic.get typeof_impls
    |> List.find_map (fun f -> f value)
    |> Option.unwrap_or_else (fun () -> failwith __LOC__)
end

type t = { shape : Shape.t }

let error () = { shape = Shape.Error.T }
let shape { shape } = shape
let print fmt { shape } = Shape.print fmt shape
let typeof { shape } = Shape.typeof shape
let init () = Shape.init ()
