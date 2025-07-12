open Std
open Kast_util

module type R = sig
  type t

  val print : formatter -> t -> unit
end

module Make (R : R) = struct
  type result = R.t

  module Shape = struct
    module T = struct
      type t = ..
    end

    include T
    include Print.Make (T)

    module Error = struct
      type T.t += T : T.t

      let init () =
        register_print (fun expr ->
            match expr with
            | T -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
            | _ -> None)
    end

    type T.t += Error = Error.T

    module Const = struct
      type T.t += T of R.t

      let init () =
        register_print (fun expr ->
            match expr with
            | T value ->
                Some
                  (fun fmt -> fprintf fmt "@{<magenta>const@} %a" R.print value)
            | _ -> None)
    end

    type T.t += Const = Const.T

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
      Const.init ();
      Binding.init ();
      Unit.init ()
  end

  type t = {
    shape : Shape.t;
    span : span;
    ty : Ty.t;
  }

  let init () = Shape.init ()
end
