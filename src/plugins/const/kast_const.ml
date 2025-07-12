open Std
open Kast_core

module ConstExpr = struct
  type t = { value : Value.t }
  type result = Value.t
  type _ Expr.t += T : t -> result Expr.t

  let eval (expr : t) (_interpreter : Interpreter.t) : result = expr.value
end

let () = Plugin.Expr.register (module ConstExpr)

module Expr = ConstExpr
