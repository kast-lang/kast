open Std
open Kast_core

module UnitTy = struct
  type t = unit
  type Ty.t += T of t

  let print fmt () = fprintf fmt "()"
end

let () = Plugin.Ty.register (module UnitTy)

module UnitValue = struct
  type t = unit
  type Value.t += T of t

  let print fmt () = fprintf fmt "()"
  let typeof () = UnitTy.T ()
end

let () = Plugin.Value.register (module UnitValue)

let () =
  Compiler.register_core_syntax
    {
      name = "unit";
      impl =
        (fun (type a) (kind : a Compiler.compilable) _ast _compiler : a ->
          match kind with
          | Expr -> Kast_const.Expr.T { value = UnitValue.T () });
    }

module Ty = UnitTy
module Value = UnitValue
