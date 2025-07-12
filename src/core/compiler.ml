open Std
module Ast = Kast_ast

type t = unit
type 'a compilable = Expr : Value.t Expr.t compilable

let init () : t = ()

type core_syntax = {
  name : string;
  impl : 'a. 'a compilable -> Ast.t -> t -> 'a;
}

let register_core_syntax : core_syntax -> unit = fun core_syntax -> ()
