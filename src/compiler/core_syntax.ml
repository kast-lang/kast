open Std
open Kast_util
open Kast_types
module Ast = Kast_ast

type handler =
  compile:(State.t -> Ast.t -> expr) -> state:State.t -> Ast.t tuple -> expr

let apply : string * handler =
  ( "apply",
    fun ~compile ~state children ->
      let f = Tuple.get_named "f" children in
      let arg = Tuple.get_named "arg" children in
      let f = compile state f in
      let arg = compile state arg in
      { shape = E_Apply { f; arg } } )

let handlers : handler StringMap.t = StringMap.of_list [ apply ]
