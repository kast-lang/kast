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
        (module C : Compiler.S)
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
            let f = C.compile Expr f in
            let arg = C.compile Expr arg in
            E_Apply { f; arg } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr ->
            (fun () -> TE_Expr (C.compile Expr ast))
            |> init_ty_expr span C.state
        | Pattern | Assignee ->
            error span "apply must be expr";
            init_error span C.state kind);
  }

let instantiate_generic : core_syntax =
  {
    name = "instantiate_generic";
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
        let generic, arg =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap2 ~unnamed:0 ~named:[ "generic"; "arg" ]
        in
        match kind with
        | Expr ->
            let generic = C.compile Expr generic in
            let arg = C.compile Expr arg in
            E_InstantiateGeneric { generic; arg } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr ->
            (fun () -> TE_Expr (C.compile Expr ast))
            |> init_ty_expr span C.state
        | Pattern | Assignee ->
            error span "instantiate_generic must be expr";
            init_error span C.state kind);
  }

(* a; b *)
let then' : core_syntax =
  {
    name = "then";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        :
        a
      ->
        let span = ast.span in
        match kind with
        | Expr ->
            let list =
              Ast.collect_list ~binary_rule_name:"core:then" ast
              |> List.map (C.compile Expr)
            in
            E_Then { list } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | _ ->
            error span "then must be expr";
            init_error span C.state kind);
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
        (module C : Compiler.S)
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
        let state = C.state |> State.enter_scope ~recursive:false in
        match kind with
        | Expr ->
            let expr = C.compile ~state Expr expr in
            E_Scope { expr } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee -> C.compile ~state Assignee expr
        | Pattern -> C.compile ~state Pattern expr
        | TyExpr -> C.compile ~state TyExpr expr);
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
            let value = C.compile PlaceExpr value in
            C.state
            |> Compiler.inject_assignee_bindings ~only_compiler:false assignee;
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
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        :
        a
      ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Placeholder |> init_assignee span C.state
        | Pattern -> P_Placeholder |> init_pattern span C.state
        | Expr -> expr_placeholder span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr ->
            (fun () -> TE_Expr (C.compile Expr ast))
            |> init_ty_expr span C.state);
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
        | PlaceExpr ->
            error span "fn_type can't be a place expr";
            init_error span C.state kind
        | TyExpr ->
            (fun () ->
              let state = C.state |> State.enter_scope ~recursive:false in
              let arg = C.compile ~state TyExpr arg in
              let result = C.compile ~state TyExpr result in
              TE_Fn { arg; result })
            |> init_ty_expr span C.state);
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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
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
            let ty : Types.ty_fn =
              {
                arg = Ty.new_not_inferred ~span:arg.span;
                result =
                  Ty.new_not_inferred
                    ~span:
                      (result
                      |> Option.map_or body.span (fun (result : Ast.t) ->
                          result.span));
              }
            in
            let def : Types.maybe_compiled_fn =
              { compiled = None; on_compiled = [] }
            in
            State.Scope.fork (fun () ->
                let state = C.state |> State.enter_scope ~recursive:false in
                let arg = C.compile ~state Pattern arg in
                ty.arg
                |> Inference.Ty.expect_inferred_as ~span:arg.data.span
                     arg.data.ty;
                state
                |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
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
                Compiler.finish_compiling def
                  { arg; body; evaled_result = result_expr };
                ty.result
                |> Inference.Ty.expect_inferred_as ~span:body.data.span
                     body.data.ty);
            E_Fn { ty; def } |> init_expr span C.state);
  }

let generic : core_syntax =
  {
    name = "generic";
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
        let body = children |> Tuple.get_named "body" |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
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
            let ty : Types.ty_fn =
              {
                arg = Ty.new_not_inferred ~span:arg.span;
                result = Ty.new_not_inferred ~span:body.span;
              }
            in
            (* TODO copypasta with fn *)
            let def : Types.maybe_compiled_fn =
              { compiled = None; on_compiled = [] }
            in
            State.Scope.fork (fun () ->
                let state = C.state |> State.enter_scope ~recursive:false in
                let arg = C.compile ~state Pattern arg in
                state
                |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
                let body = C.compile ~state Expr body in
                Compiler.finish_compiling def
                  { arg; body; evaled_result = None };
                ty.arg
                |> Inference.Ty.expect_inferred_as ~span:arg.data.span
                     arg.data.ty;
                ty.result
                |> Inference.Ty.expect_inferred_as ~span:body.data.span
                     body.data.ty);
            E_Generic { def } |> init_expr span C.state);
  }

let unit : core_syntax =
  {
    name = "unit";
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
        Tuple.assert_empty children;
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee -> A_Unit |> init_assignee span C.state
        | Pattern -> P_Unit |> init_pattern span C.state
        | Expr ->
            E_Constant (V_Unit |> Value.inferred ~span)
            |> init_expr span C.state
        | TyExpr -> (fun () -> TE_Unit) |> init_ty_expr span C.state);
  }

let type' : core_syntax =
  {
    name = "type";
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
        Tuple.assert_empty children;
        let const () =
          E_Constant (V_Ty (Ty.inferred ~span T_Ty) |> Value.inferred ~span)
          |> init_expr span C.state
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee ->
            error span "type can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "type can't be a pattern";
            init_error span C.state kind
        | Expr -> const ()
        | TyExpr -> (fun () -> TE_Expr (const ())) |> init_ty_expr span C.state);
  }

let bool_impl name value : core_syntax =
  {
    name;
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
        Tuple.assert_empty children;
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            E_Constant (V_Bool value |> Value.inferred ~span)
            |> init_expr span C.state
        | Assignee ->
            error span "%s can't be assignee" name;
            init_error span C.state kind
        | Pattern ->
            error span "%s can't be a pattern" name;
            init_error span C.state kind
        | TyExpr ->
            error span "%s can't be a type" name;
            init_error span C.state kind);
  }

let false' : core_syntax = bool_impl "false" false
let true' : core_syntax = bool_impl "true" true

let type_expr : core_syntax =
  {
    name = "type expr";
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
        let expr () = C.compile TyExpr expr in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr -> E_Ty (expr ()) |> init_expr span C.state
        | TyExpr -> expr ()
        | Assignee ->
            error span "type expr can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "type expr can't be a pattern";
            init_error span C.state kind);
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
        State.Scope.fork (fun () ->
            let expected_ty, expected_ty_expr =
              Compiler.eval_ty (module C) expected_ty
            in
            expr_data.ty
            |> Inference.Ty.expect_inferred_as ~span:expr_data.span expected_ty;

            if expr_data.ty_ascription |> Option.is_some then
              error span "there is already type ascription";
            expr_data.ty_ascription <- Some expected_ty_expr);
        expr);
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
            | PlaceExpr -> Compiler.temp_expr (module C) ast
            | Expr ->
                let path, path_expr =
                  Compiler.eval
                    ~ty:(Ty.inferred ~span:path.span T_String)
                    (module C)
                    path
                in
                let path =
                  path |> Value.expect_string
                  |> Option.unwrap_or_else (fun () ->
                      return <| init_error span C.state kind)
                in
                let uri =
                  Uri.maybe_relative_to_file span.uri (Uri.of_string path)
                in
                let path_expr =
                  Compiler.update_data Expr path_expr (fun data ->
                      { data with included_file = Some uri })
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
              Compiler.eval
                ~ty:(Ty.inferred ~span:path.span T_String)
                (module C)
                path
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
            let path_expr =
              Compiler.update_data Expr path_expr (fun data ->
                  { data with included_file = Some uri })
            in
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

let const_let (span : span) (pattern : pattern) (value_expr : expr)
    (module C : Compiler.S) =
  value_expr.data.ty
  |> Inference.Ty.expect_inferred_as ~span:value_expr.data.span pattern.data.ty;
  let interpreter_state =
    match pattern.shape with
    | P_Binding binding ->
        {
          C.state.interpreter with
          current_name_parts_rev =
            Symbol binding.name :: C.state.interpreter.current_name_parts_rev;
        }
    | _ -> C.state.interpreter
  in
  let value = Interpreter.eval interpreter_state value_expr in
  let let_expr =
    E_Assign
      {
        assignee = A_Let pattern |> init_assignee pattern.data.span C.state;
        value =
          (let const =
             E_Constant value
             |> init_expr ~evaled_exprs:[ value_expr ] value_expr.data.span
                  C.state
           in
           PE_Temp const |> init_place_expr value_expr.data.span C.state);
      }
    |> init_expr span C.state
  in
  C.state |> Compiler.inject_pattern_bindings ~only_compiler:true pattern;
  ignore @@ Interpreter.eval C.state.interpreter let_expr;
  let_expr

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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let pattern = C.compile Pattern pattern in
            let value_expr = C.compile Expr value in
            const_let span pattern value_expr (module C)
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
              Compiler.eval
                ~ty:(Ty.inferred ~span:expr.span T_String)
                (module C)
                expr
            in
            let expr_value : string =
              expr_value |> Value.expect_string
              |> Option.unwrap_or_else (fun () ->
                  return <| init_error span C.state kind)
            in
            match kind with
            | Expr ->
                E_Native { id = Id.gen (); expr = expr_value }
                |> init_expr ~evaled_exprs:[ expr ] span C.state
            | PlaceExpr ->
                error span "native must be expr, not place expr";
                init_error span C.state kind
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
        let state = C.state |> State.enter_scope ~recursive:true in
        let def = C.compile ~state Expr def in
        state.scope |> State.Scope.close;
        state.interpreter.scope |> Interpreter.Scope.close;
        match kind with
        | Expr -> E_Module { def } |> init_expr span state
        | PlaceExpr ->
            error span "module must be expr, not place expr";
            init_error span state kind
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
        with_return (fun { return } : a ->
            let span = ast.span in
            match kind with
            | Assignee ->
                A_Place (C.compile PlaceExpr ast) |> init_assignee span C.state
            | Pattern ->
                error span "dot must be expr, not pattern";
                init_error span C.state kind
            | TyExpr ->
                (fun () -> TE_Expr (C.compile Expr ast))
                |> init_ty_expr span C.state
            | Expr ->
                let place = C.compile PlaceExpr ast in
                E_Claim place |> init_expr span C.state
            | PlaceExpr -> (
                let obj, field_ast =
                  children
                  |> Tuple.map Ast.Child.expect_ast
                  |> Tuple.unwrap_named2 [ "obj"; "field" ]
                in
                let obj = C.compile PlaceExpr obj in
                let field =
                  match field_ast.shape with
                  | Simple { token = { shape = Ident ident; _ }; _ } ->
                      ident.name
                  | _ ->
                      error span "field must be ident";
                      return <| init_error span C.state kind
                in
                match obj.data.ty.var |> Inference.Var.inferred_opt with
                | Some T_CompilerScope -> (
                    let obj_expr = E_Claim obj |> init_expr span C.state in
                    let obj =
                      Kast_interpreter.eval C.state.interpreter obj_expr
                    in
                    match obj.var |> Inference.Var.inferred_opt with
                    | Some (V_CompilerScope scope) ->
                        let binding =
                          State.Scope.find_binding ~from:span field scope
                        in
                        PE_Binding binding
                        |> init_place_expr span ~evaled_exprs:[ obj_expr ]
                             C.state
                    | _ ->
                        error span "expected obj to be compiler scope";
                        return <| init_error span C.state kind)
                | _ ->
                    PE_Field
                      {
                        obj;
                        field;
                        field_span = field_ast.span;
                        label = Label.create_reference field_ast.span field;
                      }
                    |> init_place_expr span C.state)));
  }

let tuple_field (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) ({ children; _ } : Ast.group) :
    string * field_span:span * field_label:Label.t * a =
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
  | PlaceExpr -> unreachable "tuple field"
  | Expr -> (
      let value =
        match value with
        | Some value -> C.compile Expr value
        | None ->
            let place =
              PE_Binding
                (State.Scope.find_binding ~from:label_ast.span label
                   C.state.scope)
              |> init_place_expr span C.state
            in
            E_Claim place |> init_expr span C.state
      in
      match ty |> Option.map (Compiler.eval_ty (module C)) with
      | None ->
          ( label,
            ~field_span:label_ast.span,
            ~field_label:(Label.create_reference label_ast.span label),
            value )
      | Some (ty, ty_expr) ->
          value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
          ( label,
            ~field_span:label_ast.span,
            ~field_label:(Label.create_reference label_ast.span label),
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
            let place =
              PE_Binding
                (State.Scope.find_binding ~from:label_ast.span label
                   C.state.scope)
              |> init_place_expr span C.state
            in
            let expr = E_Claim place |> init_expr span C.state in
            (fun () -> TE_Expr expr) |> init_ty_expr span C.state
      in
      ( label,
        ~field_span:label_ast.span,
        ~field_label:(Label.create_definition label_ast.span label),
        value )
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
                id = Id.gen ();
                name = Symbol.create label;
                ty = Ty.new_not_inferred ~span:label_ast.span;
                span = label_ast.span;
                label = Label.create_reference label_ast.span label;
              }
            |> init_pattern span C.state
      in
      match ty |> Option.map (Compiler.eval_ty (module C)) with
      | None ->
          ( label,
            ~field_span:label_ast.span,
            ~field_label:(Label.create_reference label_ast.span label),
            value )
      | Some (ty, ty_expr) ->
          value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
          ( label,
            ~field_span:label_ast.span,
            ~field_label:(Label.create_reference label_ast.span label),
            {
              value with
              data = { value.data with ty_ascription = Some ty_expr };
            } ))

let comma_impl (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) : a =
  match kind with
  | PlaceExpr -> Compiler.temp_expr (module C) ast
  | _ -> (
      let span = ast.span in
      let children = ast |> Ast.collect_list ~binary_rule_name:"core:comma" in
      let tuple = ref Tuple.empty in
      let unnamed_idx = ref 0 in
      children
      |> List.iter (fun (child : Ast.t) ->
          match child.shape with
          | Complex { rule = { name = "core:field init"; _ }; root; _ } ->
              let name, ~field_span, ~field_label, value =
                tuple_field (module C) kind child root
              in
              tuple :=
                !tuple
                |> Tuple.add (Some name)
                     ({
                        label_span = field_span;
                        label = field_label;
                        field = value;
                      }
                       : a Types.tuple_field_of)
          | _ ->
              let field_label =
                match kind with
                | TyExpr -> Label.create_reference
                | _ -> Label.create_definition
              in
              let label = Int.to_string !unnamed_idx in
              unnamed_idx := !unnamed_idx + 1;
              tuple :=
                !tuple
                |> Tuple.add None
                     ({
                        label_span = child.span;
                        label = field_label child.span label;
                        field = C.compile kind child;
                      }
                       : a Types.tuple_field_of));
      match kind with
      | PlaceExpr -> unreachable "comma: checked earier"
      | Assignee -> A_Tuple { tuple = !tuple } |> init_assignee span C.state
      | Pattern -> P_Tuple { tuple = !tuple } |> init_pattern span C.state
      | TyExpr ->
          (fun () -> TE_Tuple { tuple = !tuple }) |> init_ty_expr span C.state
      | Expr -> E_Tuple { tuple = !tuple } |> init_expr span C.state)

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

let use : core_syntax =
  {
    name = "use";
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
            let used_expr = C.compile PlaceExpr used in
            let pattern : Pattern.Shape.t =
              match used_expr.shape with
              | PE_Binding binding ->
                  P_Binding
                    { binding with name = Symbol.create binding.name.name }
              | PE_Field { obj = _; field; field_span; label } ->
                  P_Binding
                    {
                      id = Id.gen ();
                      name = Symbol.create field;
                      span = field_span;
                      ty = used_expr.data.ty;
                      label;
                    }
              | _ ->
                  error span "Can't use this";
                  P_Placeholder
            in
            let used_expr =
              E_Claim used_expr |> init_expr used_expr.data.span C.state
            in
            let pattern = pattern |> init_pattern used_expr.data.span C.state in
            const_let span pattern used_expr (module C)
        | _ ->
            error span "use .* must be expr";
            init_error span C.state kind);
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
              Compiler.eval
                ~ty:(Ty.new_not_inferred ~span:used.span)
                (module C)
                used
            in
            let bindings =
              match Value.ty_of used |> Ty.await_inferred with
              | T_Tuple { name = _; tuple } ->
                  tuple.named |> StringMap.to_list
                  |> List.map (fun (name, field) ->
                      let field : Types.ty_tuple_field = field in
                      ({
                         id = Id.gen ();
                         name = Symbol.create name;
                         span = field.label |> Label.get_span;
                         ty = field.ty;
                         label = field.label;
                       }
                        : binding))
              | other ->
                  error span "can't use .* %a" Ty.Shape.print other;
                  []
            in
            bindings
            |> List.iter (fun binding ->
                C.state |> Compiler.inject_binding ~only_compiler:false binding);
            let result =
              E_UseDotStar
                {
                  used =
                    E_Constant used
                    |> init_expr ~evaled_exprs:[ used_expr ] used_expr.data.span
                         C.state;
                  bindings;
                }
              |> init_expr span C.state
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
              Compiler.eval
                ~ty:(Ty.new_not_inferred ~span:expr.span)
                (module C)
                expr
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
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let cond = C.compile Expr cond in
            let then_case = C.compile Expr then_case in
            let else_case = C.compile Expr else_case in
            E_If { cond; then_case; else_case } |> init_expr span C.state
        | TyExpr ->
            error span "if can't be ty expr";
            init_error span C.state kind
        | Assignee ->
            error span "if can't be assignee";
            init_error span C.state kind
        | Pattern ->
            error span "if can't be pattern";
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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
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
                  (* TODO maybe reduce copypasta here and compiling fn *)
                  let ty : Types.ty_fn =
                    {
                      arg = Ty.new_not_inferred ~span:(Span.of_ocaml __POS__);
                      result = Ty.new_not_inferred ~span:(Span.of_ocaml __POS__);
                    }
                  in
                  let def : Types.maybe_compiled_fn =
                    { compiled = None; on_compiled = [] }
                  in
                  State.Scope.fork (fun () ->
                      let state = State.enter_scope C.state ~recursive:false in
                      let arg =
                        P_Tuple
                          {
                            tuple =
                              fields
                              |> Tuple.map
                                   (fun
                                     (field : Ast.t)
                                     :
                                     pattern Types.tuple_field_of
                                   ->
                                     {
                                       label_span = field.span;
                                       label =
                                         Label.create_definition field.span
                                           (* TODO wrong span *)
                                           "<TODO>";
                                       field = C.compile ~state Pattern field;
                                     });
                          }
                        |> init_pattern name.span C.state
                      in
                      state
                      |> Compiler.inject_pattern_bindings ~only_compiler:false
                           arg;
                      let body = C.compile ~state Expr impl in
                      Compiler.finish_compiling def
                        { arg; body; evaled_result = None };
                      ty.arg
                      |> Inference.Ty.expect_inferred_as ~span:arg.data.span
                           arg.data.ty;
                      ty.result
                      |> Inference.Ty.expect_inferred_as ~span:body.data.span
                           body.data.ty);
                  E_Fn { def; ty } |> init_expr span C.state
                in
                let impl = Interpreter.eval C.state.interpreter impl_expr in
                Hashtbl.add C.state.custom_syntax_impls rule.id impl;
                E_Constant (V_Unit |> Value.inferred ~span)
                |> init_expr ~evaled_exprs:[ impl_expr ] span C.state
            | _ ->
                let name, name_expr =
                  Compiler.eval
                    ~ty:(Ty.inferred ~span:name.span T_String)
                    (module C)
                    name
                in
                let name = name |> Value.expect_string in
                let impl, impl_expr =
                  Compiler.eval
                    ~ty:
                      ((* TODO *)
                       Ty.new_not_inferred ~span:impl.span)
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
                E_Constant (V_Unit |> Value.inferred ~span)
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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let rec construct (ast : Ast.t) : expr =
              match ast.shape with
              | Ast.Error _ | Ast.Simple _ ->
                  E_Constant (V_Ast ast |> Value.inferred ~span:ast.span)
                  |> init_expr ast.span C.state
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
              {
                body =
                  C.compile
                    ~state:(State.enter_scope C.state ~recursive:false)
                    Expr body;
              }
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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let token = C.compile Pattern token in
            let state = State.enter_scope C.state ~recursive:false in
            state |> Compiler.inject_pattern_bindings ~only_compiler:false token;
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

let target_dependent : core_syntax =
  {
    name = "target_dependent";
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
        let branches =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_named "branches"
        in
        let branches =
          Ast.collect_list ~binary_rule_name:"core:union"
            ~trailing_or_leading_rule_name:"core:leading union" branches
        in
        let branches =
          branches
          |> List.filter_map (fun (branch : Ast.t) ->
              match branch.shape with
              | Complex { rule = { name = "core:fn"; _ }; root; _ } ->
                  let cond =
                    root.children |> Tuple.get_named "arg"
                    |> Ast.Child.expect_ast
                  in
                  let body =
                    root.children |> Tuple.get_named "body"
                    |> Ast.Child.expect_ast
                  in
                  let scope_with_target =
                    C.state.scope
                    |> State.Scope.inject_binding
                         ({
                            id = Id.gen ();
                            name = Types.target_symbol;
                            span;
                            ty =
                              Ty.inferred ~span:(Span.of_ocaml __POS__) T_Target;
                            label =
                              Label.create_definition span
                                Types.target_symbol.name;
                          }
                           : binding)
                  in
                  let state_with_target =
                    { C.state with scope = scope_with_target }
                  in
                  Some
                    ({
                       cond = C.compile ~state:state_with_target Expr cond;
                       body =
                         C.compile
                           ~state:(C.state |> State.enter_scope ~recursive:false)
                           Expr body;
                     }
                      : Types.expr_target_dependent_branch)
              | _ ->
                  error branch.span "target dependent branch must use fn syntax";
                  None)
        in
        match kind with
        | Expr ->
            E_TargetDependent { branches; interpreter_branch = None }
            |> init_expr span C.state
        | _ ->
            error span "target dependent must be expr";
            init_error span C.state kind);
  }

let match_ : core_syntax =
  {
    name = "match";
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
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let value, branches =
              children
              |> Tuple.map Ast.Child.expect_ast
              |> Tuple.unwrap_named2 [ "value"; "branches" ]
            in
            let value = C.compile PlaceExpr value in
            (* TODO reduce copypasta of this & target dependent *)
            let branches =
              Ast.collect_list ~binary_rule_name:"core:union"
                ~trailing_or_leading_rule_name:"core:leading union" branches
            in
            let branches =
              branches
              |> List.filter_map (fun (branch : Ast.t) ->
                  match branch.shape with
                  | Complex { rule = { name = "core:fn"; _ }; root; _ } ->
                      let pattern =
                        root.children |> Tuple.get_named "arg"
                        |> Ast.Child.expect_ast
                      in
                      let pattern = C.compile Pattern pattern in
                      let body =
                        root.children |> Tuple.get_named "body"
                        |> Ast.Child.expect_ast
                      in
                      let branch_state =
                        C.state |> State.enter_scope ~recursive:false
                      in
                      Compiler.inject_pattern_bindings ~only_compiler:false
                        pattern branch_state;
                      let body = C.compile ~state:branch_state Expr body in
                      Some ({ pattern; body } : Types.expr_match_branch)
                  | _ ->
                      error branch.span "match branch must use fn syntax";
                      None)
            in
            E_Match { value; branches } |> init_expr span C.state
        | _ ->
            error span "match must be expr";
            init_error span C.state kind);
  }

let inject_context : core_syntax =
  {
    name = "inject_context";
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
        let context_type, value =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "context_type"; "value" ]
        in
        match kind with
        | Expr -> (
            let context_ty, context_ty_expr =
              context_type
              |> Compiler.eval
                   ~ty:(Ty.inferred ~span:context_type.span T_ContextTy)
                   (module C)
            in
            match context_ty |> Value.await_inferred with
            | V_ContextTy context_ty ->
                let value = C.compile Expr value in
                E_InjectContext { context_ty; value }
                |> init_expr ~evaled_exprs:[ context_ty_expr ] span C.state
            | _ -> init_error span C.state kind)
        | _ ->
            error span "inject_context must be expr";
            init_error span C.state kind);
  }

let current_context : core_syntax =
  {
    name = "current_context";
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
        let context_type =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_named "context_type"
        in
        match kind with
        | Expr -> (
            let context_ty, context_ty_expr =
              context_type
              |> Compiler.eval
                   ~ty:(Ty.inferred ~span:context_type.span T_ContextTy)
                   (module C)
            in
            match context_ty |> Value.await_inferred with
            | V_ContextTy context_ty ->
                E_CurrentContext { context_ty }
                |> init_expr ~evaled_exprs:[ context_ty_expr ] span C.state
            | _ -> init_error span C.state kind)
        | _ ->
            error span "current_context must be expr";
            init_error span C.state kind);
  }

let binding : core_syntax =
  {
    name = "binding";
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
        let binding =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_single_unnamed
        in
        let binding, binding_expr =
          binding |> Compiler.eval ~ty:(Ty.new_not_inferred ~span) (module C)
        in
        match binding.var |> Inference.Var.inferred_opt with
        | Some (V_Binding binding) -> (
            let place =
              PE_Binding binding
              |> init_place_expr ~evaled_exprs:[ binding_expr ] span C.state
            in
            match kind with
            | PlaceExpr -> place
            | Expr -> E_Claim place |> init_expr span C.state
            | _ ->
                error span "binding must be expr (TODO)";
                init_error span C.state kind)
        | _ -> init_error span C.state kind);
  }

let __file__ : core_syntax =
  {
    name = "__FILE__";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        :
        a
      ->
        let span = ast.span in
        match kind with
        | Expr ->
            E_Constant (V_String (span.uri |> Uri.path) |> Value.inferred ~span)
            |> init_expr span C.state
        | _ ->
            error span "__FILE__ must be expr";
            init_error span C.state kind);
  }

let current_compiler_scope : core_syntax =
  {
    name = "current_compiler_scope";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        :
        a
      ->
        let span = ast.span in
        match kind with
        | Expr ->
            let scope = C.state.scope in
            (* scope.bindings
            |> StringMap.iter (fun field _binding ->
                println "@current_scope.%S" field); *)
            E_Constant (V_CompilerScope scope |> Value.inferred ~span)
            |> init_expr span C.state
        | _ ->
            error span "current_compiler_scope must be expr";
            init_error span C.state kind);
  }

let variant_impl =
 fun (type a) (module C : Compiler.S) (kind : a compiled_kind) (ast : Ast.t)
     ({ children; _ } : Ast.group) : a ->
  let label_ast = children |> Tuple.get_named "label" |> Ast.Child.expect_ast in
  let label_span = label_ast.span in
  let label =
    match label_ast.shape with
    | Simple { token = { shape = Ident ident; _ }; _ } -> ident.name
    | _ ->
        Error.error label_ast.span "field label must be ident";
        invalid_arg "tuple field"
  in
  let value_ast =
    children |> Tuple.get_named_opt "value" |> Option.map Ast.Child.expect_ast
  in
  let span = ast.span in
  match kind with
  | PlaceExpr -> Compiler.temp_expr (module C) ast
  | Expr ->
      let label = Label.create_reference label_ast.span label in
      E_Variant
        { label; label_span; value = value_ast |> Option.map (C.compile Expr) }
      |> init_expr span C.state
  | TyExpr ->
      (fun () ->
        let label = Label.create_definition label_ast.span label in
        TE_Variant
          {
            variants =
              [
                {
                  label_span;
                  label;
                  value = value_ast |> Option.map (C.compile TyExpr);
                };
              ];
          })
      |> init_ty_expr span C.state
  | Pattern ->
      let label = Label.create_reference label_ast.span label in
      P_Variant
        {
          label_span;
          label;
          value = value_ast |> Option.map (C.compile Pattern);
        }
      |> init_pattern span C.state
  | Assignee ->
      error span "variant can't be assignee";
      init_error span C.state kind

let variant_without_value : core_syntax =
  {
    name = "variant_without_value";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        :
        a
      -> variant_impl (module C) kind ast group);
  }

let union_impl (type a) (module C : Compiler.S) (kind : a compiled_kind)
    (ast : Ast.t) (_ : Ast.group) : a =
  let span = ast.span in
  let elements =
    Ast.collect_list ~binary_rule_name:"core:union"
      ~trailing_or_leading_rule_name:"core:leading union" ast
  in
  let elements = fun () -> elements |> List.map (C.compile kind) in
  match kind with
  | TyExpr ->
      (fun () ->
        let elements = elements () in
        TE_Union { elements })
      |> init_ty_expr span C.state
  | _ ->
      error span "union must be ty expr";
      init_error span C.state kind

let leading_union : core_syntax =
  {
    name = "leading union";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        :
        a
      -> union_impl (module C) kind ast group);
  }

let union : core_syntax =
  {
    name = "union";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        :
        a
      -> union_impl (module C) kind ast group);
  }

let variant : core_syntax =
  {
    name = "variant";
    handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        :
        a
      -> variant_impl (module C) kind ast group);
  }

let and_ : core_syntax =
  {
    name = "and";
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
        let lhs, rhs =
          children |> Tuple.map Ast.Child.expect_ast |> Tuple.unwrap_unnamed2
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            E_And { lhs = C.compile Expr lhs; rhs = C.compile Expr rhs }
            |> init_expr span C.state
        | _ ->
            error span "and must be expr";
            init_error span C.state kind);
  }

let or_ : core_syntax =
  {
    name = "or";
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
        let lhs, rhs =
          children |> Tuple.map Ast.Child.expect_ast |> Tuple.unwrap_unnamed2
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            E_Or { lhs = C.compile Expr lhs; rhs = C.compile Expr rhs }
            |> init_expr span C.state
        | _ ->
            error span "or must be expr";
            init_error span C.state kind);
  }

let ref_ : core_syntax =
  {
    name = "ref";
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
        let inner =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr -> E_Ref (C.compile PlaceExpr inner) |> init_expr span C.state
        | TyExpr ->
            (fun () -> TE_Ref (C.compile TyExpr inner))
            |> init_ty_expr span C.state
        | Pattern ->
            P_Ref (C.compile Pattern inner) |> init_pattern span C.state
        | Assignee ->
            error span "ref can not be assignee";
            init_error span C.state kind);
  }

let deref : core_syntax =
  {
    name = "deref";
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
        let ref =
          children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        match kind with
        | PlaceExpr ->
            PE_Deref (C.compile Expr ref) |> init_place_expr span C.state
        | Expr -> E_Claim (C.compile PlaceExpr ast) |> init_expr span C.state
        | Assignee ->
            A_Place (C.compile PlaceExpr ast) |> init_assignee span C.state
        | _ ->
            error span "deref must be expr";
            init_error span C.state kind);
  }

let impl_cast : core_syntax =
  {
    name = "impl_cast";
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
        let value, target, impl =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named3 [ "value"; "target"; "impl" ]
        in
        match kind with
        | Expr ->
            let target, target_expr =
              Compiler.eval
                ~ty:(Ty.new_not_inferred ~span:target.span)
                (module C)
                target
            in
            E_ImplCast
              {
                value = C.compile Expr value;
                target;
                impl = C.compile Expr impl;
              }
            |> init_expr ~evaled_exprs:[ target_expr ] span C.state
        | _ ->
            error span "impl cast must be expr";
            init_error span C.state kind);
  }

let cast : core_syntax =
  {
    name = "cast";
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
        let value, target =
          children
          |> Tuple.map Ast.Child.expect_ast
          |> Tuple.unwrap_named2 [ "value"; "target" ]
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
            let target, target_expr =
              Compiler.eval
                ~ty:(Ty.new_not_inferred ~span:target.span)
                (module C)
                target
            in
            E_Cast { value = C.compile Expr value; target }
            |> init_expr ~evaled_exprs:[ target_expr ] span C.state
        | _ ->
            error span "cast must be expr";
            init_error span C.state kind);
  }

let core =
  [
    apply;
    instantiate_generic;
    then';
    stmt;
    scope;
    assign;
    let';
    placeholder;
    fn_type;
    fn;
    generic;
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
    use;
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
    target_dependent;
    match_;
    inject_context;
    current_context;
    binding;
    __file__;
    current_compiler_scope;
    variant;
    variant_without_value;
    leading_union;
    union;
    and_;
    or_;
    ref_;
    deref;
    impl_cast;
    cast;
  ]

let all : core_syntax StringMap.t =
  core |> List.map (fun handler -> (handler.name, handler)) |> StringMap.of_list

let handle name (module C : Compiler.S) kind (ast : Ast.t) root =
  match all |> StringMap.find_opt name with
  | None ->
      error ast.span "there is no core syntax %S" name;
      init_error ast.span C.state kind
  | Some core_syntax -> core_syntax.handle (module C) kind ast root
