open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast

type t = {
  (* TODO do this properly *)
  mutable bindings : binding StringMap.t;
  interpreter : Interpreter.state;
}

let find_binding : Token.Shape.ident -> t -> binding =
 fun ident state ->
  match StringMap.find_opt ident.name state.bindings with
  | Some binding -> binding
  | None ->
      (* TODO fail "Could not find %S in scope" ident.name *)
      { name = ident.name; ty = Ty.new_not_inferred () }
