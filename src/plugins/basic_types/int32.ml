open Std
open Kast_core

module TyImpl = struct
  type t = unit
  type Ty.Shape.t += T of t

  let print fmt () = fprintf fmt "int32"
end

let init_ty () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = int32
  type Value.Shape.t += T of t

  let print fmt value = fprintf fmt "%ld" value
  let typeof _value = Ty.inferred (TyImpl.T ())
end

let init_value () = Plugin.Value.register (module ValueImpl)

module Ty = TyImpl
module Value = ValueImpl

let init () =
  init_ty ();
  init_value ()
