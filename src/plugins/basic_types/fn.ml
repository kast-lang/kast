open Std
open Kast_core

module TyImpl = struct
  type t = {
    arg : Ty.t;
    result : Ty.t;
  }

  type Ty.Shape.t += T of t

  let print fmt { arg; result } =
    fprintf fmt "%a -> %a" Ty.print arg Ty.print result
end

let () = Plugin.Ty.register (module TyImpl)

module ValueImpl = struct
  type t = {
    arg : Pattern.t;
    body : Expr.t;
  }

  type Value.Shape.t += T of t

  let print fmt { arg = _; body = _ } = fprintf fmt "<fn>"

  let typeof { arg; body } =
    Ty.inferred (TyImpl.T { arg = arg.ty; result = body.ty })
end

let () = Plugin.Value.register (module ValueImpl)

module Native = struct
  type t = {
    name : string;
    ty : TyImpl.t;
    impl : caller:span -> Value.t -> Value.t;
  }

  type Value.Shape.t += T of t

  let print fmt { name; ty = _; impl = _ } = fprintf fmt "<native %s>" name
  let typeof { name = _; ty; impl = _ } = Ty.inferred (TyImpl.T ty)
end

let () = Plugin.Value.register (module Native)

module Ty = TyImpl
module Value = ValueImpl
