open Std
open Kast_core

module ErrorTy = struct
  type t = unit
  type Ty.t += T of t

  let make () = ()

  let print (fmt : formatter) (_value : t) : unit =
    fprintf fmt "@{<red><error>@}"
end

let () = Plugin.Ty.register (module ErrorTy)

module ErrorValue = struct
  type t = unit
  type Value.t += T of t

  let make () = T ()

  let print (fmt : formatter) (_value : t) : unit =
    fprintf fmt "@{<red><error>@}"

  let typeof (() : t) : Ty.t = ErrorTy.T (ErrorTy.make ())
end

let () = Plugin.Value.register (module ErrorValue)

module Ty = ErrorTy
module Value = ErrorValue
