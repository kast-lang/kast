open Std
open Kast_core

module BoolTy = struct
  type t = unit
  type Ty.t += T of t

  let print fmt () = fprintf fmt "bool"
end

let () = Plugin.Ty.register (module BoolTy)

module BoolValue = struct
  type t = bool
  type Value.t += T of t

  let print fmt value = fprintf fmt "%b" value
  let typeof _value = BoolTy.T ()
end

let () = Plugin.Value.register (module BoolValue)

let () =
  Compiler.register_core_syntax
    {
      name = "true";
      impl =
        (fun (type a) (kind : a Compiler.compilable) _ast _compiler : a ->
          match kind with
          | Expr -> Kast_const.Expr.T { value = BoolValue.T true });
    }

let () =
  Compiler.register_core_syntax
    {
      name = "false";
      impl =
        (fun (type a) (kind : a Compiler.compilable) _ast _compiler : a ->
          match kind with
          | Expr -> Kast_const.Expr.T { value = BoolValue.T false });
    }

module Ty = BoolTy
module Value = BoolValue
