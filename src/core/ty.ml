open Std

module Shape = struct
  module T = struct
    type t = ..
  end

  include T
  include Print.Make (T)

  type T.t += Error : T.t

  let () =
    register_print (fun ty ->
        match ty with
        | Error -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
        | _ -> None)

  type T.t += Ty

  let () =
    register_print (fun ty ->
        match ty with
        | Ty -> Some (fun fmt -> fprintf fmt "type")
        | _ -> None)

  type T.t += Unit

  let () =
    register_print (fun ty ->
        match ty with
        | Unit -> Some (fun fmt -> fprintf fmt "()")
        | _ -> None)
end

type t = { shape : Shape.t }

let error () = { shape = Shape.Error }
let inferred shape = { shape }
let new_not_inferred () = { shape = Shape.Error }
let print fmt { shape } = Shape.print fmt shape
