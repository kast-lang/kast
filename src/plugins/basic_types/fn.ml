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
    captured : Interpreter.Scope.t;
  }

  type Value.Shape.t += T of t

  let print fmt { arg = _; body = _; captured = _ } = fprintf fmt "<fn>"

  let typeof { arg; body; captured = _ } =
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

module ExprImpl = struct
  module Apply = struct
    type t = {
      f : Expr.t;
      arg : Expr.t;
    }

    type Expr.Shape.t += T of t

    let eval span { f = f_expr; arg = arg_expr } interpreter =
      let f = interpreter |> Interpreter.Eval.expr f_expr in
      let arg = interpreter |> Interpreter.Eval.expr arg_expr in
      match f.shape with
      | ValueImpl.T { arg = arg_pattern; body; captured } ->
          let arg_matches = Interpreter.pattern_match arg_pattern arg in
          let new_state : Interpreter.t =
            {
              scope =
                Interpreter.Scope.with_values ~parent:(Some captured)
                  { by_symbol = arg_matches };
            }
          in
          let result = new_state |> Interpreter.Eval.expr body in
          result
      | Native.T { name = _; ty = _; impl } -> impl ~caller:span arg
      | _ ->
          Error.throw f_expr.span "Tried to call something thats not a fn";
          Value.error ()
  end

  let () = Plugin.Expr.register (module Apply)
end

module Ty = TyImpl
module Value = ValueImpl
module Expr = ExprImpl
