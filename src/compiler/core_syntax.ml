open Std
open Kast_util
open Kast_types
module Ast = Kast_ast

type handler =
  compile:(State.t -> Ast.t -> expr) -> state:State.t -> Ast.t tuple -> expr

let apply : string * handler =
  ( "apply",
    fun ~compile ~state children ->
      let f, arg = children |> Tuple.unwrap2 ~unnamed:0 ~named:[ "f"; "arg" ] in
      let f = compile state f in
      let arg = compile state arg in
      { shape = E_Apply { f; arg } } )

(* a; b *)
let then' : string * handler =
  ( "then",
    fun ~compile ~state children ->
      let a, b = Tuple.unwrap_unnamed2 children in
      let a = compile state a in
      let b = compile state b in
      { shape = E_Then { a; b } } )

(*  TODO remove *)
let add : string * handler =
  ( "add",
    fun ~compile ~state children ->
      let a, b = Tuple.unwrap_unnamed2 children in
      let a = compile state a in
      let b = compile state b in
      let add : value =
        {
          shape =
            V_NativeFn
              {
                name = "add";
                impl =
                  (fun arg ->
                    match arg.shape with
                    | V_Tuple { tuple } -> (
                        let a, b = Tuple.unwrap_unnamed2 tuple in
                        match (a.shape, b.shape) with
                        | V_Int32 a, V_Int32 b ->
                            let ( + ) = Int32.add in
                            { shape = V_Int32 (a + b) }
                        | _ -> fail "todo")
                    | _ -> fail "expected a tuple");
              };
        }
      in
      {
        shape =
          E_Apply
            {
              f = { shape = E_Constant add };
              arg = { shape = E_Tuple { tuple = Tuple.make [ a; b ] [] } };
            };
      } )

let handlers : handler StringMap.t = StringMap.of_list [ apply; then'; add ]
