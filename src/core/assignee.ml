open Std
open Kast_util

module Shape = struct
  module T = struct
    type t = ..
  end

  include T
  include Print.Make (T)

  module Error = struct
    type T.t += T

    let init () =
      register_print (fun expr ->
          match expr with
          | T -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
          | _ -> None)
  end

  type T.t += Error = Error.T

  module Binding = struct
    type T.t += T of Binding.t

    let init () =
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

    let init () =
      register_print (fun expr ->
          match expr with
          | T -> Some (fun fmt -> fprintf fmt "()")
          | _ -> None)
  end

  type T.t += Unit = Unit.T

  let init () =
    Error.init ();
    Binding.init ();
    Unit.init ()
end

type t = {
  shape : Shape.t;
  span : span;
  ty : Ty.t;
}

let init () = Shape.init ()
