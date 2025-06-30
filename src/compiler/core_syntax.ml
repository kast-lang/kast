open Std
open Kast_util
open Kast_types
module Ast = Kast_ast
open Init

type 'a compiled_kind = 'a Compiler.compiled_kind

type 'a handle =
  (module Compiler.S) -> 'a compiled_kind -> Ast.t tuple -> span -> 'a

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
        span
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
            E_Apply { f; arg } |> init_expr span
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
        span
        :
        a
      ->
        let a, b = Tuple.unwrap_unnamed2 children in
        match kind with
        | Expr ->
            let a = Compiler.compile Expr a in
            let b = Compiler.compile Expr b in
            E_Then { a; b } |> init_expr span
        | _ -> fail "then must be expr");
  }

(* expr; *)
let stmt : handler =
  {
    name = "stmt";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        span
        :
        a
      ->
        let expr = Tuple.unwrap_single_unnamed children in
        match kind with
        | Expr ->
            let expr = Compiler.compile Expr expr in
            E_Stmt { expr } |> init_expr span
        | _ -> fail "stmt must be expr");
  }

let scope : handler =
  {
    name = "scope";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        span
        :
        a
      ->
        let expr = Tuple.unwrap_single_unnamed children in
        match kind with
        | Expr ->
            let expr = Compiler.compile Expr expr in
            E_Scope { expr } |> init_expr span
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
        span
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
            E_Assign { assignee; value } |> init_expr span
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
        span
        :
        a
      ->
        let pattern = children |> Tuple.unwrap_single_named "pattern" in
        match kind with
        | Assignee ->
            let pattern = Compiler.compile Pattern pattern in
            A_Let pattern |> init_assignee span
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
        span
        :
        a
      ->
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Placeholder |> init_assignee span
        | Pattern -> P_Placeholder |> init_pattern span
        | Expr -> fail "todo _ expr %s" __LOC__);
  }

let fn : handler =
  {
    name = "fn";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        span
        :
        a
      ->
        let arg, body = children |> Tuple.unwrap_named2 [ "arg"; "body" ] in
        match kind with
        | Assignee -> fail "fn can't be assignee"
        | Pattern -> fail "fn can't be a pattern"
        | Expr ->
            let state = Compiler.state |> State.enter_scope in
            let arg = Compiler.compile ~state Pattern arg in
            let body = Compiler.compile ~state Expr body in
            E_Fn { arg; body } |> init_expr span);
  }

let unit : handler =
  {
    name = "unit";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        span
        :
        a
      ->
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Unit |> init_assignee span
        | Pattern -> P_Unit |> init_pattern span
        | Expr -> E_Constant { shape = V_Unit } |> init_expr span);
  }

let core = [ apply; then'; stmt; scope; assign; let'; placeholder; fn; unit ]

(*  TODO remove *)

let make_binop ~name (f : Value.shape * Value.shape -> Value.shape) : handler =
  {
    name;
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        children
        span
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
                      ty =
                        {
                          arg =
                            Ty.inferred
                            <| T_Tuple
                                 {
                                   tuple =
                                     Tuple.make
                                       (List.init 2 (fun _ ->
                                            Ty.inferred T_Int32))
                                       [];
                                 };
                          result = Ty.inferred T_Int32;
                        };
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

            E_Apply
              {
                f = E_Constant add |> init_expr (Span.fake "<binop>");
                arg =
                  E_Tuple { tuple = Tuple.make [ a; b ] [] }
                  |> init_expr (Span.fake "<binop>");
              }
            |> init_expr span
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
