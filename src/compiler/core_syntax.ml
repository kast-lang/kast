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

(* a; b *)
let scope : string * handler =
  ( "scope",
    fun ~compile ~state children ->
      let expr = Tuple.unwrap_single_unnamed children in
      let expr = compile state expr in
      { shape = E_Scope { expr } } )

(*  TODO remove *)

let make_binop ~name (f : value_shape * value_shape -> value_shape) =
  ( name,
    fun ~compile ~state children : expr ->
      let a, b = Tuple.unwrap_unnamed2 children in
      let a = compile state a in
      let b = compile state b in
      let add : value =
        {
          shape =
            V_NativeFn
              {
                name;
                impl =
                  (fun arg ->
                    match arg.shape with
                    | V_Tuple { tuple } ->
                        let a, b = Tuple.unwrap_unnamed2 tuple in
                        { shape = f (a.shape, b.shape) }
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

let core = [ apply; then'; scope ]

let todo_remove =
  let add : string * handler =
    make_binop ~name:"add" (function
      | V_Int32 a, V_Int32 b ->
          let ( + ) = Int32.add in
          V_Int32 (a + b)
      | _ -> fail "todo")
  in
  let sub : string * handler =
    make_binop ~name:"sub" (function
      | V_Int32 a, V_Int32 b ->
          let ( - ) = Int32.sub in
          V_Int32 (a - b)
      | _ -> fail "todo")
  in
  let mul : string * handler =
    make_binop ~name:"mul" (function
      | V_Int32 a, V_Int32 b ->
          let ( * ) = Int32.mul in
          V_Int32 (a * b)
      | _ -> fail "todo")
  in
  let div : string * handler =
    make_binop ~name:"div" (function
      | V_Int32 a, V_Int32 b ->
          let ( / ) = Int32.div in
          V_Int32 (a / b)
      | _ -> fail "todo")
  in
  [ add; sub; mul; div ]

let handlers : handler StringMap.t = StringMap.of_list (core @ todo_remove)
