open Std
open Kast_util
open Compiler_types
open Kast_types
module Ast = Kast_ast
open Init
open Error
module Interpreter = Kast_interpreter

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
            E_Apply { f; arg } |> init_expr span Compiler.state
        | _ ->
            error span "apply must be expr";
            init_error span Compiler.state kind);
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
            E_Then { a; b } |> init_expr span Compiler.state
        | _ ->
            error span "then must be expr";
            init_error span Compiler.state kind);
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
            E_Stmt { expr } |> init_expr span Compiler.state
        | _ ->
            error span "stmt must be expr";
            init_error span Compiler.state kind);
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
        let state = Compiler.state |> State.enter_scope in
        match kind with
        | Expr ->
            let expr = Compiler.compile ~state Expr expr in
            E_Scope { expr } |> init_expr span Compiler.state
        | Assignee -> Compiler.compile ~state Assignee expr
        | Pattern -> Compiler.compile ~state Pattern expr
        | TyExpr -> Compiler.compile ~state TyExpr expr);
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
            E_Assign { assignee; value } |> init_expr span C.state
        | _ ->
            error span "assign must be expr";
            init_error span C.state kind);
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
            A_Let pattern |> init_assignee span Compiler.state
        | _ ->
            error span "assign must be expr";
            init_error span Compiler.state kind);
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
        | Assignee -> A_Placeholder |> init_assignee span Compiler.state
        | Pattern -> P_Placeholder |> init_pattern span Compiler.state
        | Expr ->
            error span "todo _ expr %s" __LOC__;
            init_error span Compiler.state kind
        | TyExpr ->
            let ty = Ty.new_not_inferred () in
            let value : value = { shape = V_Ty ty } in
            let const : expr =
              E_Constant value |> init_expr span Compiler.state
            in
            TE_Expr const |> init_ty_expr span Compiler.state);
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
        | Assignee ->
            error span "fn_type can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "fn_type can't be a pattern";
            init_error span C.state kind
        | Expr ->
            error span "fn_type can't be a expr";
            init_error span C.state kind
        | TyExpr ->
            let state = C.state |> State.enter_scope in
            let arg = C.compile ~state TyExpr arg in
            let result = C.compile ~state TyExpr result in
            TE_Fn { arg; result } |> init_ty_expr span C.state);
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
        | Assignee ->
            error span "fn can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "fn can't be a pattern";
            init_error span C.state kind
        | TyExpr ->
            error span "fn can't be a ty";
            init_error span C.state kind
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
            E_Fn { arg; body; evaled_result = result_expr }
            |> init_expr span C.state);
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
        | Assignee -> A_Unit |> init_assignee span Compiler.state
        | Pattern -> P_Unit |> init_pattern span Compiler.state
        | Expr -> E_Constant { shape = V_Unit } |> init_expr span Compiler.state
        | TyExpr -> TE_Unit |> init_ty_expr span Compiler.state);
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
        let const =
          E_Constant { shape = V_Ty ty_ty } |> init_expr span Compiler.state
        in
        match kind with
        | Assignee ->
            error span "type can't be assignee";
            init_error span Compiler.state kind
        | Pattern ->
            error span "type can't be a pattern";
            init_error span Compiler.state kind
        | Expr -> const
        | TyExpr -> TE_Expr const |> init_ty_expr span Compiler.state);
  }

let false' : core_syntax =
  {
    name = "false";
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
        let const =
          E_Constant { shape = V_Bool false } |> init_expr span Compiler.state
        in
        match kind with
        | Expr -> const
        | Assignee ->
            error span "false can't be assignee";
            init_error span Compiler.state kind
        | Pattern ->
            error span "false can't be a pattern";
            init_error span Compiler.state kind
        | TyExpr ->
            error span "false can't be a type";
            init_error span Compiler.state kind);
  }

let true' : core_syntax =
  {
    name = "true";
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
        let const =
          E_Constant { shape = V_Bool true } |> init_expr span Compiler.state
        in
        match kind with
        | Expr -> const
        | Assignee ->
            error span "true can't be assignee";
            init_error span Compiler.state kind
        | Pattern ->
            error span "true can't be a pattern";
            init_error span Compiler.state kind
        | TyExpr ->
            error span "true can't be a type";
            init_error span Compiler.state kind);
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
        | Expr -> E_Ty expr |> init_expr span Compiler.state
        | TyExpr -> expr
        | Assignee ->
            error span "type expr can't be assignee";
            init_error span Compiler.state kind
        | Pattern ->
            error span "type expr can't be a pattern";
            init_error span Compiler.state kind);
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
        with_return (fun { return } : a ->
            let span = ast.span in
            let path =
              children
              |> Tuple.unwrap_single_named "path"
              |> Ast.Child.expect_ast
            in
            match kind with
            | Expr ->
                let path, path_expr =
                  Compiler.eval ~ty:(Ty.inferred T_String) (module C) path
                in
                let path =
                  path |> Value.expect_string
                  |> Option.unwrap_or_else (fun () ->
                         return <| init_error span C.state kind)
                in
                let uri =
                  Uri.maybe_relative_to_file span.uri (Uri.of_string path)
                in
                let imported_value : value =
                  Compiler.import ~span (module C) uri
                in
                E_Constant imported_value
                |> init_expr ~evaled_exprs:[ path_expr ] span C.state
            | Assignee ->
                error span "Can't assign to import";
                init_error span C.state kind
            | Pattern ->
                error span "import can't be a pattern";
                init_error span C.state kind
            | TyExpr ->
                error span "Type imports not supported (TODO)";
                init_error span C.state kind));
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
        with_return (fun { return } ->
            let span = ast.span in
            let path =
              children
              |> Tuple.unwrap_single_named "path"
              |> Ast.Child.expect_ast
            in
            let path, path_expr =
              Compiler.eval ~ty:(Ty.inferred T_String) (module C) path
            in
            let path =
              path |> Value.expect_string
              |> Option.unwrap_or_else (fun () ->
                     return <| init_error span C.state kind)
            in
            let uri =
              Uri.maybe_relative_to_file span.uri (Uri.of_string path)
            in
            Effect.perform (CompilerEffect.FileStartedProcessing uri);
            let source = Source.read uri in
            let parsed : Kast_parser.result =
              Kast_parser.parse source Kast_default_syntax.ruleset
            in
            let ast =
              parsed.ast
              |> Option.unwrap_or_else (fun () : Ast.t ->
                     error span "included file is empty";
                     { shape = Ast.Error { parts = [] }; span })
            in
            let compiled = C.compile kind ast in
            Effect.perform
              (CompilerEffect.FileIncluded
                 {
                   root = C.state.currently_compiled_file |> Option.get;
                   uri;
                   parsed;
                   kind;
                   compiled;
                 });
            Compiler.update_data kind compiled (fun data ->
                { data with evaled_exprs = path_expr :: data.evaled_exprs })));
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
                  assignee =
                    A_Let pattern |> init_assignee pattern.data.span C.state;
                  value =
                    E_Constant value
                    |> init_expr ~evaled_exprs:[ value_expr ]
                         value_expr.data.span C.state;
                }
              |> init_expr span C.state
            in
            C.state |> Compiler.inject_pattern_bindings pattern;
            ignore @@ Interpreter.eval C.state.interpreter let_expr;
            let_expr
        | Assignee ->
            error span "const must be expr, not assignee expr";
            init_error span C.state kind
        | Pattern ->
            error span "const must be expr, not pattern";
            init_error span C.state kind
        | TyExpr ->
            error span "const must be expr, not type expr";
            init_error span C.state kind);
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
        with_return (fun { return } ->
            let span = ast.span in
            let expr =
              children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
            in
            let expr_value, expr =
              Compiler.eval ~ty:(Ty.inferred T_String) (module C) expr
            in
            let expr_value : string =
              expr_value |> Value.expect_string
              |> Option.unwrap_or_else (fun () ->
                     return <| init_error span C.state kind)
            in
            match kind with
            | Expr ->
                E_Native { expr = expr_value }
                |> init_expr ~evaled_exprs:[ expr ] span C.state
            | Assignee ->
                error span "native must be expr, not assignee expr";
                init_error span C.state kind
            | Pattern ->
                error span "native must be expr, not pattern";
                init_error span C.state kind
            | TyExpr ->
                error span "native must be expr, not type expr";
                init_error span C.state kind));
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
        (* TODO recursive scope *)
        let state = C.state |> State.enter_scope in
        let def = C.compile ~state Expr def in
        match kind with
        | Expr -> E_Module { def } |> init_expr span state
        | Assignee ->
            error span "module must be expr, not assignee expr";
            init_error span state kind
        | Pattern ->
            error span "module must be expr, not pattern";
            init_error span state kind
        | TyExpr ->
            error span "module must be expr, not type expr";
            init_error span state kind);
  }

let dot : core_syntax =
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
        with_return (fun { return } ->
            let span = ast.span in
            match kind with
            | Assignee ->
                error span "todo assign to field";
                init_error span C.state kind
            | Pattern ->
                error span "dot must be expr, not pattern";
                init_error span C.state kind
            | TyExpr ->
                TE_Expr (C.compile Expr ast) |> init_ty_expr span C.state
            | Expr ->
                let obj, field =
                  children
                  |> Tuple.map Ast.Child.expect_ast
                  |> Tuple.unwrap_named2 [ "obj"; "field" ]
                in
                let obj = C.compile Expr obj in
                let field =
                  match field.shape with
                  | Simple { token = { shape = Ident ident; _ }; _ } ->
                      ident.name
                  | _ ->
                      error span "field must be ident";
                      return <| init_error span C.state kind
                in
                E_Field { obj; field } |> init_expr span C.state));
  }

let tuple_field (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) ({ children; _ } : Ast.group) : string * a =
  let span = ast.span in
  let label_ast = children |> Tuple.get_named "label" |> Ast.Child.expect_ast in
  let label =
    match label_ast.shape with
    | Simple { token = { shape = Ident ident; _ }; _ } -> ident.name
    | _ ->
        Error.error label_ast.span "field label must be ident";
        invalid_arg "tuple field"
  in
  let ty =
    children |> Tuple.get_named_opt "type"
    |> Option.map (fun ty ->
           let group = ty |> Ast.Child.expect_group in
           group.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast)
  in
  let value =
    children
    |> Tuple.get_named_opt "value"
    |> Option.map (fun value ->
           let group = value |> Ast.Child.expect_group in
           group.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast)
  in
  match kind with
  | Expr -> (
      let value =
        match value with
        | Some value -> C.compile Expr value
        | None ->
            E_Binding
              (State.Scope.find_binding ~from:label_ast.span label C.state.scope)
            |> init_expr span C.state
      in
      match ty |> Option.map (Compiler.eval_ty (module C)) with
      | None -> (label, value)
      | Some (ty, ty_expr) ->
          value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
          ( label,
            {
              value with
              data = { value.data with ty_ascription = Some ty_expr };
            } ))
  | TyExpr ->
      (match value with
      | None -> ()
      | Some _ -> error span "unexpected value");
      let value =
        match ty with
        | Some value -> C.compile TyExpr value
        | None ->
            let expr =
              E_Binding
                (State.Scope.find_binding ~from:label_ast.span label
                   C.state.scope)
              |> init_expr span C.state
            in
            TE_Expr expr |> init_ty_expr span C.state
      in
      (label, value)
  | Assignee ->
      error span "todo %s" __LOC__;
      invalid_arg "todo"
  | Pattern -> (
      let value =
        match value with
        | Some value -> C.compile Pattern value
        | None ->
            P_Binding
              {
                name = Symbol.create label;
                ty = Ty.new_not_inferred ();
                references = [];
                span = label_ast.span;
              }
            |> init_pattern span C.state
      in
      match ty |> Option.map (Compiler.eval_ty (module C)) with
      | None -> (label, value)
      | Some (ty, ty_expr) ->
          value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
          ( label,
            {
              value with
              data = { value.data with ty_ascription = Some ty_expr };
            } ))

let comma_impl (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) : a =
  let span = ast.span in
  let children = ast |> Ast.collect_list ~binary_rule_name:"core:comma" in
  let tuple = ref Tuple.empty in
  children
  |> List.iter (fun (child : Ast.t) ->
         match child.shape with
         | Complex { rule = { name = "core:field init"; _ }; root; _ } ->
             let name, value = tuple_field (module C) kind child root in
             tuple := !tuple |> Tuple.add (Some name) value
         | _ -> tuple := !tuple |> Tuple.add None (C.compile kind child));
  match kind with
  | Assignee ->
      error span "todo comma assignee";
      init_error span C.state kind
  | Pattern -> P_Tuple { tuple = !tuple } |> init_pattern span C.state
  | TyExpr -> TE_Tuple { tuple = !tuple } |> init_ty_expr span C.state
  | Expr -> E_Tuple { tuple = !tuple } |> init_expr span C.state

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
              | other ->
                  error span "can't use .* %a" Ty.Shape.print other;
                  []
            in
            bindings
            |> List.iter (fun binding ->
                   C.state |> Compiler.inject_binding binding);
            let result =
              E_UseDotStar
                { used = E_Constant used |> init_expr span C.state; bindings }
              |> init_expr ~evaled_exprs:[ used_expr ] span C.state
            in
            ignore @@ Interpreter.eval C.state.interpreter result;
            result
        | _ ->
            error span "use .* must be expr";
            init_error span C.state kind);
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
            E_Constant value
            |> init_expr ~evaled_exprs:[ value_expr ] span C.state
        | _ ->
            error span "comptime must be expr";
            init_error span C.state kind);
  }

let if' : core_syntax =
  {
    name = "if";
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
        let cond, then_case, else_case =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named3 [ "cond"; "then_case"; "else_case" ]
        in
        let cond = C.compile Expr cond in
        match kind with
        | Expr ->
            let then_case = C.compile Expr then_case in
            let else_case = C.compile Expr else_case in
            E_If { cond; then_case; else_case } |> init_expr span C.state
        | TyExpr ->
            error span "if can't be assignee";
            init_error span C.state kind
        | Assignee ->
            error span "if can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "if can't be assignee";
            init_error span C.state kind);
  }

let impl_syntax : core_syntax =
  {
    name = "impl syntax";
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
        | Expr -> (
            let name, impl =
              children
              |> Tuple.map Ast.Child.expect_ast
              |> Tuple.unwrap_named2 [ "name"; "impl" ]
            in
            match name.shape with
            | Complex
                { rule = { name = "core:scope"; _ }; root = { children; _ } } ->
                let inner =
                  children |> Tuple.unwrap_single_unnamed
                  |> Ast.Child.expect_ast
                  |> function
                  | { shape = Complex inner; _ } -> inner
                  | _ -> failwith __LOC__
                in
                let rule = inner.rule in
                let fields =
                  inner.root.children |> Tuple.map Ast.Child.expect_ast
                in
                let impl_expr =
                  let state = State.enter_scope C.state in
                  let arg =
                    P_Tuple
                      {
                        tuple =
                          fields
                          |> Tuple.map (fun field ->
                                 C.compile ~state Pattern field);
                      }
                    |> init_pattern name.span C.state
                  in
                  state |> Compiler.inject_pattern_bindings arg;
                  let body = C.compile ~state Expr impl in
                  E_Fn { arg; body; evaled_result = None }
                  |> init_expr span C.state
                in
                let impl = Interpreter.eval C.state.interpreter impl_expr in
                Hashtbl.add C.state.custom_syntax_impls rule.id impl;
                E_Constant { shape = V_Unit }
                |> init_expr ~evaled_exprs:[ impl_expr ] span C.state
            | _ ->
                let name, name_expr =
                  Compiler.eval ~ty:(Ty.inferred T_String) (module C) name
                in
                let name = name |> Value.expect_string in
                let impl, impl_expr =
                  Compiler.eval
                    ~ty:
                      ((* TODO *)
                       Ty.new_not_inferred ())
                    (module C)
                    impl
                in
                (match name with
                | Some name -> (
                    Kast_default_syntax.ruleset
                    |> Kast_parser.Ruleset.find_rule_opt name
                    |> function
                    | Some rule ->
                        Hashtbl.add C.state.custom_syntax_impls rule.id impl
                    | None -> Error.error span "Syntax rule not found: %S" name)
                | None ->
                    Error.error name_expr.data.span "Name must be a string");
                E_Constant { shape = V_Unit }
                |> init_expr ~evaled_exprs:[ name_expr; impl_expr ] span C.state
            )
        | TyExpr ->
            error span "impl syntax can't be assignee";
            init_error span C.state kind
        | Assignee ->
            error span "impl syntax can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "impl syntax can't be assignee";
            init_error span C.state kind);
  }

let field_init : core_syntax =
  {
    name = "field init";
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

let quote : core_syntax =
  {
    name = "quote";
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
        let body =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        match kind with
        | Expr ->
            let rec construct (ast : Ast.t) : expr =
              match ast.shape with
              | Ast.Error _ | Ast.Simple _ ->
                  E_Constant { shape = V_Ast ast } |> init_expr ast.span C.state
              | Ast.Complex { rule; root } when rule.name = "core:unquote" ->
                  let unquote =
                    root.children |> Tuple.unwrap_single_unnamed
                    |> Ast.Child.expect_ast
                  in
                  C.compile kind unquote
              | Ast.Complex { rule; root } ->
                  E_QuoteAst { rule; root = construct_group root }
                  |> init_expr ast.span C.state
              | Ast.Syntax _ -> fail "TODO"
            and construct_group (group : Ast.group) : Expr.Shape.quote_ast_group
                =
              {
                rule = group.rule;
                children =
                  group.children
                  |> Tuple.map
                       (fun (child : Ast.child) : Expr.Shape.quote_ast_child ->
                         match child with
                         | Group child_group ->
                             Group (construct_group child_group)
                         | Ast child -> Ast (construct child));
              }
            in
            construct body
        | _ ->
            error span "quote must be expr";
            init_error span C.state kind);
  }

let loop : core_syntax =
  {
    name = "loop";
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
        let body =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        match kind with
        | Expr ->
            E_Loop
              { body = C.compile ~state:(State.enter_scope C.state) Expr body }
            |> init_expr span C.state
        | _ ->
            error span "loop must be expr";
            init_error span C.state kind);
  }

let unwindable : core_syntax =
  {
    name = "unwindable";
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
        let token, body =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "token"; "body" ]
        in
        match kind with
        | Expr ->
            let token = C.compile Pattern token in
            let state = State.enter_scope C.state in
            state |> Compiler.inject_pattern_bindings token;
            let body = C.compile ~state Expr body in
            E_Unwindable { token; body } |> init_expr span C.state
        | _ ->
            error span "unwindable must be expr";
            init_error span C.state kind);
  }

let unwind : core_syntax =
  {
    name = "unwind";
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
        let token, value =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "token"; "value" ]
        in
        match kind with
        | Expr ->
            E_Unwind
              { token = C.compile Expr token; value = C.compile Expr value }
            |> init_expr span C.state
        | _ ->
            error span "unwind must be expr";
            init_error span C.state kind);
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
    true';
    false';
    if';
    impl_syntax;
    field_init;
    quote;
    loop;
    unwindable;
    unwind;
  ]

let all : core_syntax StringMap.t =
  core |> List.map (fun handler -> (handler.name, handler)) |> StringMap.of_list

let handle name (module C : Compiler.S) kind (ast : Ast.t) root =
  match all |> StringMap.find_opt name with
  | None ->
      error ast.span "there is no core syntax %S" name;
      init_error ast.span C.state kind
  | Some core_syntax -> core_syntax.handle (module C) kind ast root
