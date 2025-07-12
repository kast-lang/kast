open Std
open Kast_core

module TyImpl = struct
  type t = unit
  type Ty.Shape.t += T of t

  let print fmt () = fprintf fmt "string"
end

let () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = string
  type Value.Shape.t += T of t

  let print fmt value = fprintf fmt "%S" value
  let typeof _value = Ty.inferred (TyImpl.T ())
end

let () = Plugin.Value.register (module ValueImpl)

module Ty = TyImpl
module Value = ValueImpl
