open Std
open Kast_core

module TyImpl = struct
  type t = { tuple : Ty.t tuple }
  type Ty.Shape.t += T of t

  let print fmt { tuple } = fprintf fmt "%a" (Tuple.print Ty.print) tuple
end

let () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = { tuple : Value.t tuple }
  type Value.Shape.t += T of t

  let print fmt { tuple } = fprintf fmt "%a" (Tuple.print Value.print) tuple

  let typeof { tuple } =
    Ty.inferred (TyImpl.T { tuple = tuple |> Tuple.map Value.typeof })
end

let () = Plugin.Value.register (module ValueImpl)

module Ty = TyImpl
module Value = ValueImpl
