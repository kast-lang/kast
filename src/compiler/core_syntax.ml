open Std
open Kast_util
open Kast_types
module Ast = Kast_ast
open Init
open Error
module Interpreter = Kast_interpreter

type 'a compiled_kind = 'a Compiler.compiled_kind

type 'a handle =
  (module Compiler.S) -> 'a compiled_kind -> Ast.t -> Ast.group -> 'a

type core_syntax = {
  name : string;
  handle : 'a. 'a handle;
}

let apply : core_syntax =
  {
    name = "apply";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let f, arg =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap2 ~unnamed:0 ~named:[ "f"; "arg" ]
        in
        match kind with
        | Expr ->
            let f = Compiler.compile Expr f in
            let arg = Compiler.compile Expr arg in
            E_Apply { f; arg } |> init_expr span
        | _ -> error span "apply must be expr");
  }

(* a; b *)
let then' : core_syntax =
  {
    name = "then";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let a, b =
          children |> Tuple.map Ast.Child.expect_ast |> Tuple.unwrap_unnamed2
        in
        match kind with
        | Expr ->
            let a = Compiler.compile Expr a in
            let b = Compiler.compile Expr b in
            E_Then { a; b } |> init_expr span
        | _ -> error span "then must be expr");
  }

(* expr; *)
let stmt : core_syntax =
  {
    name = "stmt";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let expr =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_unnamed
        in
        match kind with
        | Expr ->
            let expr = Compiler.compile Expr expr in
            E_Stmt { expr } |> init_expr span
        | _ -> error span "stmt must be expr");
  }

let scope : core_syntax =
  {
    name = "scope";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let expr =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_unnamed
        in
        match kind with
        | Expr ->
            let expr = Compiler.compile Expr expr in
            E_Scope { expr } |> init_expr span
        | Assignee -> Compiler.compile Assignee expr
        | Pattern -> Compiler.compile Pattern expr
        | TyExpr -> Compiler.compile TyExpr expr);
  }

let assign : core_syntax =
  {
    name = "assign";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let assignee, value =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "assignee"; "value" ]
        in
        match kind with
        | Expr ->
            let assignee = C.compile Assignee assignee in
            let value = C.compile Expr value in
            C.state |> Compiler.inject_assignee_bindings assignee;
            E_Assign { assignee; value } |> init_expr span
        | _ -> error span "assign must be expr");
  }

let let' : core_syntax =
  {
    name = "let";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let pattern =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_named "pattern"
        in
        match kind with
        | Assignee ->
            let pattern = Compiler.compile Pattern pattern in
            A_Let pattern |> init_assignee span
        | _ -> error span "assign must be expr");
  }

let placeholder : core_syntax =
  {
    name = "placeholder";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Placeholder |> init_assignee span
        | Pattern -> P_Placeholder |> init_pattern span
        | Expr -> error span "todo _ expr %s" __LOC__
        | TyExpr ->
            let ty = Ty.new_not_inferred () in
            let value : value = { shape = V_Ty ty } in
            let const : expr = E_Constant value |> init_expr span in
            TE_Expr const |> init_ty_expr span);
  }

let fn_type : core_syntax =
  {
    name = "fn_type";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let arg = children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
        let context =
          children
          |> Tuple.get_named_opt "context"
          |> Option.map (fun (child : Ast.child) ->
                 let group = child |> Ast.Child.expect_group in
                 group.children |> Tuple.unwrap_single_unnamed
                 |> Ast.Child.expect_ast)
        in
        let result =
          children |> Tuple.get_named "result" |> Ast.Child.expect_ast
        in
        match kind with
        | Assignee -> error span "fn_type can't be assignee"
        | Pattern -> error span "fn_type can't be a pattern"
        | Expr -> error span "fn_type can't be a expr"
        | TyExpr ->
            let state = C.state |> State.enter_scope in
            let arg = C.compile ~state TyExpr arg in
            let result = C.compile ~state TyExpr result in
            TE_Fn { arg; result } |> init_ty_expr span);
  }

let fn : core_syntax =
  {
    name = "fn";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let arg = children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
        let context =
          children
          |> Tuple.get_named_opt "context"
          |> Option.map (fun (child : Ast.child) ->
                 let group = child |> Ast.Child.expect_group in
                 group.children |> Tuple.unwrap_single_unnamed
                 |> Ast.Child.expect_ast)
        in
        let result =
          children
          |> Tuple.get_named_opt "result"
          |> Option.map (fun (child : Ast.child) ->
                 let group = child |> Ast.Child.expect_group in
                 group.children |> Tuple.unwrap_single_unnamed
                 |> Ast.Child.expect_ast)
        in
        let body = children |> Tuple.get_named "body" |> Ast.Child.expect_ast in
        match kind with
        | Assignee -> error span "fn can't be assignee"
        | Pattern -> error span "fn can't be a pattern"
        | TyExpr -> error span "fn can't be a ty"
        | Expr ->
            let state = C.state |> State.enter_scope in
            let arg = C.compile ~state Pattern arg in
            state |> Compiler.inject_pattern_bindings arg;
            let body = C.compile ~state Expr body in
            let result_expr =
              result
              |> Option.map (fun result ->
                     let result, result_expr =
                       Compiler.eval_ty (module C) result
                     in
                     body.data.ty
                     |> Inference.Ty.expect_inferred_as ~span:body.data.span
                          result;
                     result_expr)
            in
            E_Fn { arg; body; evaled_result = result_expr } |> init_expr span);
  }

let unit : core_syntax =
  {
    name = "unit";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Unit |> init_assignee span
        | Pattern -> P_Unit |> init_pattern span
        | Expr -> E_Constant { shape = V_Unit } |> init_expr span
        | TyExpr -> TE_Unit |> init_ty_expr span);
  }

let type' : core_syntax =
  {
    name = "type";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        Tuple.assert_empty children;
        let ty_ty = Ty.inferred T_Ty in
        let const = E_Constant { shape = V_Ty ty_ty } |> init_expr span in
        match kind with
        | Assignee -> error span "type can't be assignee"
        | Pattern -> error span "type can't be a pattern"
        | Expr -> const
        | TyExpr -> TE_Expr const |> init_ty_expr span);
  }

let type_expr : core_syntax =
  {
    name = "type expr";
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let expr =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let expr = Compiler.compile TyExpr expr in
        match kind with
        | Assignee -> error span "type expr can't be assignee"
        | Pattern -> error span "type expr can't be a pattern"
        | Expr -> E_Ty expr |> init_expr span
        | TyExpr -> expr);
  }

let type_ascribe : core_syntax =
  {
    name = "type ascribe";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let expr, expected_ty =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "expr"; "type" ]
        in
        let expr = C.compile kind expr in
        let expr_data = Compiler.get_data kind expr in
        let expected_ty, expected_ty_expr =
          Compiler.eval_ty (module C) expected_ty
        in
        expr_data.ty
        |> Inference.Ty.expect_inferred_as ~span:expr_data.span expected_ty;
        Compiler.update_data kind expr (fun data ->
            if data.ty_ascription |> Option.is_some then
              error span "there is already type ascription";
            { data with ty_ascription = Some expected_ty_expr }));
  }

let import : core_syntax =
  {
    name = "import";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let path =
          children |> Tuple.unwrap_single_named "path" |> Ast.Child.expect_ast
        in
        match kind with
        | Expr ->
            let path, path_expr =
              Compiler.eval ~ty:(Ty.inferred T_String) (module C) path
            in
            let path = path |> Value.expect_string in
            let uri =
              Uri.maybe_relative_to_file span.uri (Uri.of_string path)
            in
            let imported_value : value = Compiler.import ~span (module C) uri in
            E_Constant imported_value
            |> init_expr ~evaled_exprs:[ path_expr ] span
        | Assignee -> error span "Can't assign to import"
        | Pattern -> error span "import can't be a pattern"
        | TyExpr -> error span "Type imports not supported (TODO)");
  }

let include' : core_syntax =
  {
    name = "include";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let path =
          children |> Tuple.unwrap_single_named "path" |> Ast.Child.expect_ast
        in
        let path, path_expr =
          Compiler.eval ~ty:(Ty.inferred T_String) (module C) path
        in
        let path = path |> Value.expect_string in
        let uri = Uri.maybe_relative_to_file span.uri (Uri.of_string path) in
        let source = Source.read uri in
        let parsed : Kast_parser.result =
          Kast_parser.parse source Kast_default_syntax.ruleset
        in
        let ast =
          parsed.ast
          |> Option.unwrap_or_else (fun () ->
                 error span "included file is empty")
        in
        let compiled = C.compile kind ast in
        Effect.perform
          (Compiler.Effect.FileIncluded { uri; parsed; kind; compiled });
        Compiler.update_data kind compiled (fun data ->
            { data with evaled_exprs = path_expr :: data.evaled_exprs }));
  }

let const : core_syntax =
  {
    name = "const";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let pattern, value =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "pattern"; "value" ]
        in
        match kind with
        | Expr ->
            let pattern = C.compile Pattern pattern in
            let value_expr = C.compile Expr value in
            value_expr.data.ty
            |> Inference.Ty.expect_inferred_as ~span:value_expr.data.span
                 pattern.data.ty;
            let value = Interpreter.eval C.state.interpreter value_expr in
            let let_expr =
              E_Assign
                {
                  assignee = A_Let pattern |> init_assignee pattern.data.span;
                  value =
                    E_Constant value
                    |> init_expr ~evaled_exprs:[ value_expr ]
                         value_expr.data.span;
                }
              |> init_expr span
            in
            C.state |> Compiler.inject_pattern_bindings pattern;
            ignore @@ Interpreter.eval C.state.interpreter let_expr;
            let_expr
        | Assignee -> error span "const must be expr, not assignee expr"
        | Pattern -> error span "const must be expr, not pattern"
        | TyExpr -> error span "const must be expr, not type expr");
  }

let native : core_syntax =
  {
    name = "native";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let expr =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let expr_value, expr =
          Compiler.eval ~ty:(Ty.inferred T_String) (module C) expr
        in
        let expr_value : string = expr_value |> Value.expect_string in
        match kind with
        | Expr ->
            E_Native { expr = expr_value }
            |> init_expr ~evaled_exprs:[ expr ] span
        | Assignee -> error span "native must be expr, not assignee expr"
        | Pattern -> error span "native must be expr, not pattern"
        | TyExpr -> error span "native must be expr, not type expr");
  }

let module' : core_syntax =
  {
    name = "module";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let def =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let def = C.compile Expr def in
        match kind with
        | Expr -> E_Module { def } |> init_expr span
        | Assignee -> error span "module must be expr, not assignee expr"
        | Pattern -> error span "module must be expr, not pattern"
        | TyExpr -> error span "module must be expr, not type expr");
  }

let rec dot : core_syntax =
  {
    name = ".";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        match kind with
        | Assignee -> error span "todo assign to field"
        | Pattern -> error span "dot must be expr, not pattern"
        | TyExpr -> TE_Expr (C.compile Expr ast) |> init_ty_expr span
        | Expr ->
            let obj, field =
              children
              |> Tuple.map Ast.Child.expect_ast
              |> Tuple.unwrap_named2 [ "obj"; "field" ]
            in
            let obj = C.compile Expr obj in
            let field =
              match field.shape with
              | Simple { token = { shape = Ident ident; _ }; _ } -> ident.name
              | _ -> error span "field must be ident"
            in
            E_Field { obj; field } |> init_expr span);
  }

let comma_impl (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) : a =
  let span = ast.span in
  let children = ast |> Ast.collect_list ~binary_rule_name:"core:comma" in
  let children = children |> List.map (fun child -> C.compile kind child) in
  match kind with
  | Assignee -> error span "todo comma assignee"
  | Pattern -> error span "todo comma pattern"
  | TyExpr -> error span "todo comma ty expr"
  | Expr -> E_Tuple { tuple = Tuple.make children [] } |> init_expr span

let comma : core_syntax =
  {
    name = "comma";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        :
        a
      -> comma_impl (module C) kind ast);
  }

let trailing_comma : core_syntax =
  {
    name = "trailing comma";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (_ : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let ast =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        comma_impl (module C) kind ast);
  }

let use_dot_star : core_syntax =
  {
    name = "use .*";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let used =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let span = ast.span in
        match kind with
        | Expr ->
            let used, used_expr =
              Compiler.eval ~ty:(Ty.new_not_inferred ()) (module C) used
            in
            let bindings =
              match (Value.ty_of used).var |> Inference.Var.await_inferred with
              | T_Tuple { tuple } ->
                  tuple.named |> StringMap.to_list
                  |> List.map (fun (name, field_ty) ->
                         let field_span =
                           (* TODO *)
                           span
                         in
                         ({
                            name = Symbol.create name;
                            span = field_span;
                            ty = field_ty;
                            references = [];
                          }
                           : binding))
              | other -> error span "can't use .* %a" Ty.Shape.print other
            in
            bindings
            |> List.iter (fun binding ->
                   C.state |> Compiler.inject_binding binding);
            let result =
              E_UseDotStar
                { used = E_Constant used |> init_expr span; bindings }
              |> init_expr ~evaled_exprs:[ used_expr ] span
            in
            ignore @@ Interpreter.eval C.state.interpreter result;
            result
        | _ -> error span "use .* must be expr");
  }

let comptime : core_syntax =
  {
    name = "comptime";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let expr =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let span = ast.span in
        match kind with
        | Expr ->
            let value, value_expr =
              Compiler.eval ~ty:(Ty.new_not_inferred ()) (module C) expr
            in
            E_Constant value |> init_expr ~evaled_exprs:[ value_expr ] span
        | _ -> error span "comptime must be expr");
  }

let core =
  [
    apply;
    then';
    stmt;
    scope;
    assign;
    let';
    placeholder;
    fn_type;
    fn;
    unit;
    type';
    type_expr;
    type_ascribe;
    import;
    include';
    const;
    native;
    module';
    dot;
    comma;
    trailing_comma;
    use_dot_star;
    comptime;
  ]

(*  TODO remove *)

let make_binop ~name (f : Value.shape * Value.shape -> Value.shape) :
    core_syntax =
  {
    name;
    handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        let a, b =
          children |> Tuple.map Ast.Child.expect_ast |> Tuple.unwrap_unnamed2
        in
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
                          | _ -> error span "expected a tuple");
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
        | _ -> error span "bin op must be expr");
  }

let todo_remove =
  let add : core_syntax =
    make_binop ~name:"add" (function
      | V_Int32 a, V_Int32 b ->
          let ( + ) = Int32.add in
          V_Int32 (a + b)
      | _ -> fail "todo %s" __LOC__)
  in
  let sub : core_syntax =
    make_binop ~name:"sub" (function
      | V_Int32 a, V_Int32 b ->
          let ( - ) = Int32.sub in
          V_Int32 (a - b)
      | _ -> fail "todo %s" __LOC__)
  in
  let mul : core_syntax =
    make_binop ~name:"mul" (function
      | V_Int32 a, V_Int32 b ->
          let ( * ) = Int32.mul in
          V_Int32 (a * b)
      | _ -> fail "todo %s" __LOC__)
  in
  let div : core_syntax =
    make_binop ~name:"div" (function
      | V_Int32 a, V_Int32 b ->
          let ( / ) = Int32.div in
          V_Int32 (a / b)
      | _ -> fail "todo %s" __LOC__)
  in
  [ add; sub; mul; div ]

let all : core_syntax StringMap.t =
  core @ todo_remove
  |> List.map (fun handler -> (handler.name, handler))
  |> StringMap.of_list

let handle name compiler kind (ast : Ast.t) root =
  match all |> StringMap.find_opt name with
  | None -> error ast.span "there is no core syntax %S" name
  | Some core_syntax -> core_syntax.handle compiler kind ast root
