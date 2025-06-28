open Std
open Kast_util
open Kast_types
module Ast = Kast_ast

type 'a compiled_kind = 'a Compiler.compiled_kind
type 'a handle = (module Compiler.S) -> 'a compiled_kind -> Ast.t tuple -> 'a

type handler = {
  name : string;
  handle : 'a. 'a handle;
}

let apply : handler =
  {
    name = "apply";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let f, arg =
          children |> Tuple.unwrap2 ~unnamed:0 ~named:[ "f"; "arg" ]
        in
        match kind with
        | Expr ->
            let f = Compiler.compile Expr f in
            let arg = Compiler.compile Expr arg in
            { shape = E_Apply { f; arg } }
        | _ -> fail "apply must be expr");
  }

(* a; b *)
let then' : handler =
  {
    name = "then";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let a, b = Tuple.unwrap_unnamed2 children in
        match kind with
        | Expr ->
            let a = Compiler.compile Expr a in
            let b = Compiler.compile Expr b in
            { shape = E_Then { a; b } }
        | _ -> fail "then must be expr");
  }

let scope : handler =
  {
    name = "scope";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let expr = Tuple.unwrap_single_unnamed children in
        match kind with
        | Expr ->
            let expr = Compiler.compile Expr expr in
            { shape = E_Scope { expr } }
        | Assignee -> Compiler.compile Assignee expr
        | Pattern -> Compiler.compile Pattern expr);
  }

let assign : handler =
  {
    name = "assign";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let assignee, value =
          children |> Tuple.unwrap_named2 [ "assignee"; "value" ]
        in
        match kind with
        | Expr ->
            let assignee = Compiler.compile Assignee assignee in
            let value = Compiler.compile Expr value in
            { shape = E_Assign { assignee; value } }
        | _ -> fail "assign must be expr");
  }

let let' : handler =
  {
    name = "let";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let pattern = children |> Tuple.unwrap_single_named "pattern" in
        match kind with
        | Assignee ->
            let pattern = Compiler.compile Pattern pattern in
            { shape = A_Let pattern }
        | _ -> fail "assign must be expr");
  }

let placeholder : handler =
  {
    name = "placeholder";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        Tuple.assert_empty children;
        match kind with
        | Assignee -> { shape = A_Placeholder }
        | Pattern -> { shape = P_Placeholder }
        | Expr -> fail "todo _ expr %s" __LOC__);
  }

let core = [ apply; then'; scope; assign; let'; placeholder ]

(*  TODO remove *)

let make_binop ~name (f : Value.shape * Value.shape -> Value.shape) : handler =
  {
    name;
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        :
        a
      ->
        let a, b = Tuple.unwrap_unnamed2 children in
        match kind with
        | Expr ->
            let a = Compiler.compile Expr a in
            let b = Compiler.compile Expr b in
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
            }
        | _ -> fail "bin op must be expr");
  }

let todo_remove =
  let add : handler =
    make_binop ~name:"add" (function
      | V_Int32 a, V_Int32 b ->
          let ( + ) = Int32.add in
          V_Int32 (a + b)
      | _ -> fail "todo %s" __LOC__)
  in
  let sub : handler =
    make_binop ~name:"sub" (function
      | V_Int32 a, V_Int32 b ->
          let ( - ) = Int32.sub in
          V_Int32 (a - b)
      | _ -> fail "todo %s" __LOC__)
  in
  let mul : handler =
    make_binop ~name:"mul" (function
      | V_Int32 a, V_Int32 b ->
          let ( * ) = Int32.mul in
          V_Int32 (a * b)
      | _ -> fail "todo %s" __LOC__)
  in
  let div : handler =
    make_binop ~name:"div" (function
      | V_Int32 a, V_Int32 b ->
          let ( / ) = Int32.div in
          V_Int32 (a / b)
      | _ -> fail "todo %s" __LOC__)
  in
  [ add; sub; mul; div ]

let handlers : handler StringMap.t =
  core @ todo_remove
  |> List.map (fun handler -> (handler.name, handler))
  |> StringMap.of_list
