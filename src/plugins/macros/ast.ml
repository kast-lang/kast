open Std
open Kast_core

module TyImpl = struct
  type t = unit
  type Ty.Shape.t += T of t

  let print fmt () = fprintf fmt "ast"
end

let () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = Ast.t
  type Value.Shape.t += T of t

  let print = Ast.print
  let typeof _ast = Ty.inferred (TyImpl.T ())
end

let () = Plugin.Value.register (module ValueImpl)

module Ty = TyImpl
module Value = ValueImpl
