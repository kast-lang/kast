open Std
open Kast_core

module TyI = struct
  type t = unit
  type Ty.t += T of t

  let print fmt () = fprintf fmt "int32"
end

let () = Plugin.Ty.register (module TyI)

module ValueI = struct
  type t = int32
  type Value.t += T of t

  let print fmt value = fprintf fmt "%ld" value
  let typeof _value = TyI.T ()
end

let () = Plugin.Value.register (module ValueI)

module Ty = TyI
module Value = ValueI
