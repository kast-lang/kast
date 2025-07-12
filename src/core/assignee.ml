open Std
open Kast_util

module Shape = struct
  module T = struct
    type t = ..
  end

  include T
  include Print.Make (T)

  type T.t += Error : T.t

  let () =
    register_print (fun expr ->
        match expr with
        | Error -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
        | _ -> None)

  module Binding = struct
    type T.t += T of Binding.t

    let () =
      register_print (fun expr ->
          match expr with
          | T binding ->
              Some
                (fun fmt ->
                  fprintf fmt "@{<magenta>binding@} %a" Binding.print binding)
          | _ -> None)
  end

  type T.t += Binding = Binding.T

  module Unit = struct
    type T.t += T

    let () =
      register_print (fun expr ->
          match expr with
          | T -> Some (fun fmt -> fprintf fmt "()")
          | _ -> None)
  end

  type T.t += Unit = Unit.T
end

type t = {
  shape : Shape.t;
  span : span;
  ty : Ty.t;
}
