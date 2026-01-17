open Std
open Kast_util
open Compiler_types
open Kast_types
module Ast = Kast_ast
open Init
open Error
module Interpreter = Kast_interpreter

let const_shape = Types.const_shape

type 'a handle = (module Compiler.S) -> 'a compiled_kind -> Ast.t -> Ast.group -> 'a

type core_syntax =
  { name : string
  ; handle : 'a. 'a handle
  }

let apply : core_syntax =
  { name = "apply"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let f, arg =
          children
          |> Ast.flatten_children
          |> Tuple.unwrap2 ~unnamed:0 ~named:[ "f"; "arg" ]
        in
        match kind with
        | Expr ->
          let f = C.compile Expr f in
          let arg = C.compile Expr arg in
          E_Apply { f; arg } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr -> (fun () -> TE_Expr (C.compile Expr ast)) |> init_ty_expr span C.state
        | Pattern | Assignee ->
          error span "apply must be expr";
          init_error span C.state kind)
  }
;;

let instantiate_generic : core_syntax =
  { name = "instantiate_generic"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let generic, arg =
          children
          |> Ast.flatten_children
          |> Tuple.unwrap2 ~unnamed:0 ~named:[ "generic"; "arg" ]
        in
        match kind with
        | Expr ->
          let generic = C.compile Expr generic in
          let arg = C.compile Expr arg in
          E_InstantiateGeneric { generic; arg } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr -> (fun () -> TE_Expr (C.compile Expr ast)) |> init_ty_expr span C.state
        | Pattern | Assignee ->
          error span "instantiate_generic must be expr";
          init_error span C.state kind)
  }
;;

(* a; b *)
let then' : core_syntax =
  { name = "then"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        : a ->
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
          init_error span C.state kind)
  }
;;

(* expr; *)
let stmt : core_syntax =
  { name = "stmt"
  ; handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr = children |> Ast.flatten_children |> Tuple.unwrap_single_unnamed in
        match kind with
        | Expr ->
          let expr = Compiler.compile Expr expr in
          E_Stmt { expr } |> init_expr span Compiler.state
        | _ ->
          error span "stmt must be expr";
          init_error span Compiler.state kind)
  }
;;

let scope : core_syntax =
  { name = "scope"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr = children |> Ast.flatten_children |> Tuple.unwrap_single_unnamed in
        let state = C.state |> State.enter_scope ~span:expr.span ~recursive:false in
        match kind with
        | Expr ->
          let expr = C.compile ~state Expr expr in
          E_Scope { expr } |> init_expr span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee -> C.compile ~state Assignee expr
        | Pattern -> C.compile ~state Pattern expr
        | TyExpr -> C.compile ~state TyExpr expr)
  }
;;

let assign : core_syntax =
  { name = "assign"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let assignee, value =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "assignee"; "value" ]
        in
        match kind with
        | Expr ->
          let assignee = C.compile Assignee assignee in
          let value = C.compile PlaceExpr value in
          C.state |> Compiler.inject_assignee_bindings ~only_compiler:false assignee;
          E_Assign { assignee; value } |> init_expr span C.state
        | _ ->
          error span "assign must be expr";
          init_error span C.state kind)
  }
;;

let let' : core_syntax =
  { name = "let"
  ; handle =
      (fun (type a)
        (module Compiler : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let pattern =
          children |> Ast.flatten_children |> Tuple.unwrap_single_named "pattern"
        in
        match kind with
        | Assignee ->
          let pattern = Compiler.compile Pattern pattern in
          A_Let pattern |> init_assignee span Compiler.state
        | _ ->
          error span "assign must be expr";
          init_error span Compiler.state kind)
  }
;;

let placeholder : core_syntax =
  { name = "placeholder"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | Assignee -> A_Placeholder |> init_assignee span C.state
        | Pattern -> P_Placeholder |> init_pattern span C.state
        | Expr -> expr_placeholder span C.state
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr -> (fun () -> TE_Expr (C.compile Expr ast)) |> init_ty_expr span C.state)
  }
;;

let fn_type : core_syntax =
  { name = "fn_type"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let arg = children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
        let context =
          children
          |> Tuple.get_named_opt "context"
          |> Option.map (fun (child : Ast.child) ->
            let group = child |> Ast.Child.expect_group in
            group.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast)
        in
        let result = children |> Tuple.get_named "result" |> Ast.Child.expect_ast in
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
            let state = C.state |> State.enter_scope ~span ~recursive:false in
            let arg = C.compile ~state TyExpr arg in
            let result = C.compile ~state TyExpr result in
            TE_Fn { arg; result })
          |> init_ty_expr span C.state)
  }
;;

let fn : core_syntax =
  { name = "fn"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let arg = children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
        let context =
          children
          |> Tuple.get_named_opt "context"
          |> Option.map (fun (child : Ast.child) ->
            let group = child |> Ast.Child.expect_group in
            group.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast)
        in
        let result_ty =
          children
          |> Tuple.get_named_opt "result"
          |> Option.map (fun (child : Ast.child) ->
            let group = child |> Ast.Child.expect_group in
            group.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast)
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
          let scope = State.var_scope C.state in
          let ty : Types.ty_fn =
            { arg = Ty.new_not_inferred ~scope ~span:arg.span
            ; result =
                Ty.new_not_inferred
                  ~scope
                  ~span:
                    (result_ty
                     |> Option.map_or body.span (fun (result : Ast.t) -> result.span))
            }
          in
          let def : Types.maybe_compiled_fn =
            { span; compiled = None; on_compiled = [] }
          in
          let result_fn_expr = E_Fn { ty; def } |> init_expr span C.state in
          State.Scope.fork (fun () ->
            let state = C.state |> State.enter_scope ~span ~recursive:false in
            Log.trace (fun log -> log "starting to compile fn at %a" Span.print span);
            let arg = C.compile ~state Pattern arg in
            Log.trace (fun log ->
              log
                "compiled fn arg = %a at %a"
                (Pattern.print ~options:{ types = false; spans = false })
                arg
                Span.print
                span);
            ty.arg |> Inference.Ty.expect_inferred_as ~span:arg.data.span arg.data.ty;
            state |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
            let body = C.compile ~state Expr body in
            Log.trace (fun log ->
              log
                "compiled fn body (ty = %a) at %a"
                Ty.print
                body.data.ty
                Span.print
                body.data.span);
            let result_ty_expr =
              result_ty
              |> Option.map (fun result_ty ->
                let result_ty, result_expr = Compiler.eval_ty (module C) result_ty in
                Log.trace (fun log ->
                  log
                    "evaled fn expected result ty = %a at %a"
                    Ty.print
                    result_ty
                    Span.print
                    body.data.span);
                Log.trace (fun log ->
                  log "unifying %a and %a" Ty.print body.data.ty Ty.print result_ty);
                body.data.ty
                |> Inference.Ty.expect_inferred_as ~span:body.data.span result_ty;
                Log.trace (fun log ->
                  log "unified result ty and body ty at %a" Span.print body.data.span);
                Log.trace (fun log ->
                  log "after unifying %a and %a" Ty.print body.data.ty Ty.print result_ty);
                result_expr, result_ty)
            in
            (match result_ty_expr with
             | None -> ()
             | Some result_ty_expr ->
               result_fn_expr |> Compiler.data_append TyExpr result_ty_expr Expr);
            ty.result |> Inference.Ty.expect_inferred_as ~span:body.data.span body.data.ty;
            Compiler.finish_compiling def { arg; body };
            Log.trace (fun log ->
              log
                "finished compiling fn at %a, result ty = %a"
                Span.print
                span
                Ty.print
                ty.result));
          result_fn_expr)
  }
;;

let generic : core_syntax =
  { name = "generic"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let arg = children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
        let body = children |> Tuple.get_named "body" |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee ->
          error span "generic can't be assignee";
          init_error span C.state kind
        | Pattern ->
          error span "generic can't be a pattern";
          init_error span C.state kind
        | TyExpr ->
          let (module C : Compiler.S) =
            Compiler.update_module
              (module C)
              (C.state |> State.enter_scope ~span ~recursive:false)
          in
          let arg = C.compile Pattern arg in
          C.state |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
          let result_ty, result_expr = Compiler.eval_ty (module C) body in
          let generic_ty =
            T_Generic (Interpreter.generic_ty ~span arg result_ty) |> Ty.inferred ~span
          in
          let generic_ty = V_Ty generic_ty |> Value.inferred ~span in
          let expr =
            const_shape generic_ty
            |> init_expr span C.state
            |> Compiler.data_add Pattern arg Expr
            |> Compiler.data_add TyExpr (result_expr, result_ty) Expr
          in
          (fun () -> TE_Expr expr) |> init_ty_expr span C.state
        | Expr ->
          let def : Types.maybe_compiled_fn =
            { span; compiled = None; on_compiled = [] }
          in
          let inner_state = C.state |> State.enter_scope ~span ~recursive:false in
          let arg = C.compile ~state:inner_state Pattern arg in
          inner_state |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
          let ty =
            Interpreter.generic_ty
              ~span
              arg
              (Ty.new_not_inferred ~scope:(State.var_scope inner_state) ~span:body.span)
          in
          State.Scope.fork (fun () ->
            let body = C.compile ~state:inner_state Expr body in
            Compiler.finish_compiling def { arg; body };
            Log.trace (fun log -> log "ty.result = %a" Ty.print ty.result);
            Log.trace (fun log -> log "body.data.ty = %a" Ty.print body.data.ty);
            ty.result |> Inference.Ty.expect_inferred_as ~span:body.data.span body.data.ty);
          let result = E_Generic { def; ty } |> init_expr span C.state in
          result)
  }
;;

let unit : core_syntax =
  { name = "unit"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Assignee -> A_Unit |> init_assignee span C.state
        | Pattern -> P_Unit |> init_pattern span C.state
        | Expr -> const_shape (V_Unit |> Value.inferred ~span) |> init_expr span C.state
        | TyExpr -> (fun () -> TE_Unit) |> init_ty_expr span C.state)
  }
;;

let type' : core_syntax =
  { name = "type"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        Tuple.assert_empty children;
        let const () =
          const_shape (V_Ty (Ty.inferred ~span T_Ty) |> Value.inferred ~span)
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
        | TyExpr -> (fun () -> TE_Expr (const ())) |> init_ty_expr span C.state)
  }
;;

let bool_impl name value : core_syntax =
  { name
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        Tuple.assert_empty children;
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          const_shape (V_Bool value |> Value.inferred ~span) |> init_expr span C.state
        | Assignee ->
          error span "%s can't be assignee" name;
          init_error span C.state kind
        | Pattern ->
          error span "%s can't be a pattern" name;
          init_error span C.state kind
        | TyExpr ->
          error span "%s can't be a type" name;
          init_error span C.state kind)
  }
;;

let false' : core_syntax = bool_impl "false" false
let true' : core_syntax = bool_impl "true" true

let type_expr : core_syntax =
  { name = "type expr"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
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
          init_error span C.state kind)
  }
;;

let newtype : core_syntax =
  { name = "newtype"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr -> E_Newtype (C.compile TyExpr expr) |> init_expr span C.state
        | TyExpr ->
          error span "newtype can't be type expr";
          init_error span C.state kind
        | Assignee ->
          error span "newtype can't be assignee";
          init_error span C.state kind
        | Pattern ->
          error span "newtype can't be a pattern";
          init_error span C.state kind)
  }
;;

let type_ascribe : core_syntax =
  { name = "type ascribe"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr, expected_ty =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "expr"; "type" ]
        in
        let expr = C.compile kind expr in
        let expr_data = Compiler.get_data kind expr in
        State.Scope.fork (fun () ->
          Log.trace (fun log -> log "Evaling ascription at %a" Span.print span);
          let expected_ty, expected_ty_expr = Compiler.eval_ty (module C) expected_ty in
          Log.trace (fun log ->
            log
              "Evaled ascription at %a = %a (scope=%a)"
              Span.print
              span
              Ty.print
              expected_ty
              Print.print_var_scope
              (Inference.Var.scope expected_ty.var));
          expr_data.ty |> Inference.Ty.expect_inferred_as ~span:expr_data.span expected_ty;
          let _ : a =
            expr |> Compiler.data_add TyExpr (expected_ty_expr, expected_ty) kind
          in
          (Compiler.get_data kind expr).evaled.ty_ascribed <- true);
        expr)
  }
;;

let import : core_syntax =
  { name = "import"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        with_return (fun { return } : a ->
          let span = ast.span in
          let path =
            children |> Tuple.unwrap_single_named "path" |> Ast.Child.expect_ast
          in
          match kind with
          | PlaceExpr -> Compiler.temp_expr (module C) ast
          | Expr ->
            let path_value, path_expr =
              Compiler.eval ~ty:(Ty.inferred ~span:path.span T_String) (module C) path
            in
            let path =
              path_value
              |> Value.expect_string
              |> Option.unwrap_or_else (fun () -> return <| init_error span C.state kind)
            in
            let uri = Uri.maybe_relative_to_file span.uri (Uri.of_string path) in
            let path_expr =
              Compiler.update_data Expr path_expr (fun data ->
                { data with included_file = Some uri })
            in
            let imported_value : value = Compiler.import ~span (module C) uri in
            const_shape imported_value
            |> init_expr span C.state
            |> Compiler.data_add Expr (path_expr, path_value) Expr
          | Assignee ->
            error span "Can't assign to import";
            init_error span C.state kind
          | Pattern ->
            error span "import can't be a pattern";
            init_error span C.state kind
          | TyExpr ->
            error span "Type imports not supported (TODO)";
            init_error span C.state kind))
  }
;;

let include' : core_syntax =
  { name = "include"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        with_return (fun { return } ->
          let span = ast.span in
          let path =
            children |> Tuple.unwrap_single_named "path" |> Ast.Child.expect_ast
          in
          let path_value, path_expr =
            Compiler.eval ~ty:(Ty.inferred ~span:path.span T_String) (module C) path
          in
          let path =
            path_value
            |> Value.expect_string
            |> Option.unwrap_or_else (fun () -> return <| init_error span C.state kind)
          in
          let uri = Uri.maybe_relative_to_file span.uri (Uri.of_string path) in
          Effect.perform (CompilerEffect.FileStartedProcessing uri);
          let path_expr =
            Compiler.update_data Expr path_expr (fun data ->
              { data with included_file = Some uri })
          in
          let source = Source.read uri in
          let parsed : Kast_parser.result =
            Kast_parser.parse source Kast_default_syntax.ruleset
          in
          let compiled = C.compile kind parsed.ast in
          Effect.perform
            (CompilerEffect.FileIncluded
               { root = C.state.currently_compiled_file |> Option.get
               ; uri
               ; parsed
               ; kind
               ; compiled
               });
          compiled |> Compiler.data_add Expr (path_expr, path_value) kind))
  }
;;

let eval_const ~async (state : State.t) (expr : expr) : value =
  let result =
    Value.new_not_inferred_of_ty
      ~scope:(State.var_scope state)
      ~span:expr.data.span
      expr.data.ty
  in
  let invoke = if async then State.Scope.fork else fun f -> f () in
  invoke (fun () ->
    let value = Kast_interpreter.eval state.interpreter expr in
    result |> Inference.Value.expect_inferred_as ~span:expr.data.span value);
  result
;;

let const_let
      (span : span)
      (pattern : pattern)
      (value_expr : expr)
      (module C : Compiler.S)
  =
  value_expr.data.ty
  |> Inference.Ty.expect_inferred_as ~span:value_expr.data.span pattern.data.ty;
  let interpreter_state =
    match pattern.shape with
    | P_Binding { bind_mode = Claim; binding } ->
      { C.state.interpreter with
        current_name = Concat (C.state.interpreter.current_name, Symbol binding.name)
      }
    | _ -> C.state.interpreter
  in
  (* TODO async true *)
  let value =
    eval_const ~async:false { C.state with interpreter = interpreter_state } value_expr
  in
  let let_expr =
    E_Assign
      { assignee = A_Let pattern |> init_assignee pattern.data.span C.state
      ; value =
          (let const =
             const_shape value
             |> init_expr value_expr.data.span C.state
             |> Compiler.data_add Expr (value_expr, value) Expr
           in
           PE_Temp const |> init_place_expr value_expr.data.span C.state)
      }
    |> init_expr span C.state
  in
  C.state |> Compiler.inject_pattern_bindings ~only_compiler:true pattern;
  ignore @@ Interpreter.eval C.state.interpreter let_expr;
  let_expr
;;

let const : core_syntax =
  { name = "const"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let pattern, value =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "pattern"; "value" ]
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let pattern = C.compile Pattern pattern in
          let new_state =
            match pattern.shape with
            | P_Binding { bind_mode = Claim; binding } ->
              { C.state with
                interpreter =
                  { C.state.interpreter with
                    current_name =
                      Concat (C.state.interpreter.current_name, Symbol binding.name)
                  }
              }
            | _ -> C.state
          in
          let value_expr = C.compile ~state:new_state Expr value in
          const_let span pattern value_expr (module C)
        | Assignee ->
          error span "const must be expr, not assignee expr";
          init_error span C.state kind
        | Pattern ->
          error span "const must be expr, not pattern";
          init_error span C.state kind
        | TyExpr ->
          error span "const must be expr, not type expr";
          init_error span C.state kind)
  }
;;

let native : core_syntax =
  { name = "native"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        with_return (fun { return } ->
          let span = ast.span in
          let expr = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
          let expr_value, expr =
            Compiler.eval ~ty:(Ty.inferred ~span:expr.span T_String) (module C) expr
          in
          let expr_string : string =
            expr_value
            |> Value.expect_string
            |> Option.unwrap_or_else (fun () -> return <| init_error span C.state kind)
          in
          match kind with
          | Expr ->
            E_Native { id = Id.gen (); expr = expr_string }
            |> init_expr span C.state
            |> Compiler.data_add Expr (expr, expr_value) kind
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
            init_error span C.state kind))
  }
;;

let module' : core_syntax =
  { name = "module"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let def = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let state = C.state |> State.enter_scope ~span ~recursive:true in
        let def = C.compile ~state Expr def in
        let bindings = state.scope.bindings |> StringMap.to_list |> List.map snd in
        Kast_profiling.record
          (fun () -> "closing module compiler scope")
          (fun () -> state.scope |> State.Scope.close);
        Kast_profiling.record
          (fun () -> "closing module interpreter scope")
          (fun () -> state.interpreter.scope |> Interpreter.Scope.close);
        match kind with
        | Expr -> E_Module { def; bindings } |> init_expr span state
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
          init_error span state kind)
  }
;;

let dot : core_syntax =
  { name = "."
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        with_return (fun { return } : a ->
          let span = ast.span in
          match kind with
          | Assignee -> A_Place (C.compile PlaceExpr ast) |> init_assignee span C.state
          | Pattern ->
            error span "dot must be expr, not pattern";
            init_error span C.state kind
          | TyExpr ->
            (fun () -> TE_Expr (C.compile Expr ast)) |> init_ty_expr span C.state
          | Expr ->
            let place = C.compile PlaceExpr ast in
            E_Claim place |> init_expr span C.state
          | PlaceExpr ->
            let obj, field_ast =
              children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "obj"; "field" ]
            in
            let obj = C.compile PlaceExpr obj in
            let field : Types.field_expr =
              match field_ast.shape with
              | Simple { token = { shape = Ident ident; _ }; _ } ->
                Name (Label.create_reference field_ast.span ident.name)
              | Simple { token = { shape = Number { raw; _ }; _ }; _ } ->
                (match raw |> int_of_string_opt with
                 | Some i -> Index i
                 | None ->
                   error span "Failed to parse as member";
                   return <| init_error span C.state kind)
              | Complex _ -> Expr (C.compile Expr field_ast)
              | _ ->
                error span "field must be ident";
                return <| init_error span C.state kind
            in
            (match obj.data.ty.var |> Inference.Var.inferred_opt with
             | Some T_CompilerScope ->
               let obj_expr = E_Claim obj |> init_expr span C.state in
               let obj = Kast_interpreter.eval C.state.interpreter obj_expr in
               (match Kast_interpreter.eval_field_expr C.state.interpreter field with
                | Error () -> return <| init_error span C.state kind
                | Ok member ->
                  let field =
                    match member with
                    | Index i -> Int.to_string i
                    | Name s -> s
                  in
                  (match obj.var |> Inference.Var.inferred_opt with
                   | Some (V_CompilerScope scope) ->
                     let binding =
                       State.Scope.find_binding
                         ~from_scope:(State.var_scope C.state)
                         ~from:span
                         field
                         scope
                     in
                     PE_Binding binding
                     |> init_place_expr span C.state
                     |> Compiler.data_add Expr (obj_expr, obj) kind
                   | _ ->
                     error span "expected obj to be compiler scope";
                     return <| init_error span C.state kind))
             | _ ->
               PE_Field { obj; field; field_span = field_ast.span }
               |> init_place_expr span C.state)))
  }
;;

let tuple_field
      (type a)
      (module C : Compiler.S)
      (kind : a compiled_kind)
      (ast : Ast.t)
      ({ children; _ } : Ast.group)
  : string * field_span:span * field_label:Label.t option * a
  =
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
    children
    |> Tuple.get_named_opt "type"
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
  | Expr ->
    let value =
      match value with
      | Some value -> C.compile Expr value
      | None ->
        let place =
          PE_Binding
            (State.Scope.find_binding
               ~from_scope:(State.var_scope C.state)
               ~from:label_ast.span
               label
               C.state.scope)
          |> init_place_expr span C.state
        in
        E_Claim place |> init_expr span C.state
    in
    (match ty |> Option.map (Compiler.eval_ty (module C)) with
     | None ->
       ( label
       , ~field_span:label_ast.span
       , ~field_label:(Some (Label.create_reference label_ast.span label))
       , value )
     | Some (ty, ty_expr) ->
       value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
       ( label
       , ~field_span:label_ast.span
       , ~field_label:(Some (Label.create_reference label_ast.span label))
       , value |> Compiler.data_add TyExpr (ty_expr, ty) kind ))
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
            (State.Scope.find_binding
               ~from_scope:(State.var_scope C.state)
               ~from:label_ast.span
               label
               C.state.scope)
          |> init_place_expr span C.state
        in
        let expr = E_Claim place |> init_expr span C.state in
        (fun () -> TE_Expr expr) |> init_ty_expr span C.state
    in
    ( label
    , ~field_span:label_ast.span
    , ~field_label:(Some (Label.create_definition label_ast.span label))
    , value )
  | Assignee ->
    error span "todo %s" __LOC__;
    invalid_arg "todo"
  | Pattern ->
    let value =
      match value with
      | Some value -> C.compile Pattern value
      | None ->
        let scope = State.var_scope C.state in
        P_Binding
          { bind_mode = Claim
          ; binding =
              { id = Id.gen ()
              ; scope
              ; name = Symbol.create label
              ; ty = Ty.new_not_inferred ~scope ~span:label_ast.span
              ; span = label_ast.span
              ; label = Label.create_reference label_ast.span label
              ; mut = false
              }
          }
        |> init_pattern span C.state
    in
    (match ty |> Option.map (Compiler.eval_ty (module C)) with
     | None ->
       ( label
       , ~field_span:label_ast.span
       , ~field_label:(Some (Label.create_reference label_ast.span label))
       , value )
     | Some (ty, ty_expr) ->
       value.data.ty |> Inference.Ty.expect_inferred_as ~span ty;
       ( label
       , ~field_span:label_ast.span
       , ~field_label:(Some (Label.create_reference label_ast.span label))
       , value |> Compiler.data_add TyExpr (ty_expr, ty) kind ))
;;

let comma_impl (type a) (module C : Compiler.S) (kind : a compiled_kind) (ast : Ast.t) : a
  =
  match kind with
  | PlaceExpr -> Compiler.temp_expr (module C) ast
  | _ ->
    let span = ast.span in
    let children = ast |> Ast.collect_list ~binary_rule_name:"core:comma" in
    let parts_rev : a Types.tuple_part_of list ref = ref [] in
    let unnamed_idx = ref 0 in
    children
    |> List.iter (fun (child : Ast.t) ->
      match child.shape with
      | Complex { rule = { name = "core:field init"; _ }; root; _ } ->
        let name, ~field_span, ~field_label, value =
          tuple_field (module C) kind child root
        in
        let part : a Types.tuple_part_of =
          Field { label_span = field_span; label = field_label; field = value }
        in
        parts_rev := part :: !parts_rev
      | Complex { rule = { name = "core:unpack"; _ }; root; _ } ->
        let packed =
          root.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
        in
        let part : a Types.tuple_part_of = Unpack (C.compile kind packed) in
        parts_rev := part :: !parts_rev
      | _ ->
        unnamed_idx := !unnamed_idx + 1;
        let part : a Types.tuple_part_of =
          Field { label_span = child.span; label = None; field = C.compile kind child }
        in
        parts_rev := part :: !parts_rev);
    (match kind with
     | PlaceExpr -> unreachable "comma: checked earier"
     | Assignee ->
       A_Tuple { parts = !parts_rev |> List.rev } |> init_assignee span C.state
     | Pattern -> P_Tuple { parts = !parts_rev |> List.rev } |> init_pattern span C.state
     | TyExpr ->
       (fun () -> TE_Tuple { parts = !parts_rev |> List.rev })
       |> init_ty_expr span C.state
     | Expr -> E_Tuple { parts = !parts_rev |> List.rev } |> init_expr span C.state)
;;

let comma : core_syntax =
  { name = "comma"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        : a -> comma_impl (module C) kind ast)
  }
;;

let trailing_comma : core_syntax =
  { name = "trailing comma"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (_ : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let ast = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        comma_impl (module C) kind ast)
  }
;;

let use : core_syntax =
  { name = "use"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let used = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let span = ast.span in
        match kind with
        | Expr ->
          let used_expr = C.compile PlaceExpr used in
          let pattern : Pattern.Shape.t =
            match used_expr.shape with
            | PE_Binding binding ->
              P_Binding
                { bind_mode = Claim
                ; binding =
                    { id = Id.gen ()
                    ; scope = binding.scope
                    ; name = Symbol.create binding.name.name
                    ; span = used_expr.data.span
                    ; ty = binding.ty
                    ; label = binding.label
                    ; mut = false
                    }
                }
            | PE_Field { obj = _; field; field_span } ->
              (match field with
               | Name label ->
                 P_Binding
                   { bind_mode = Claim
                   ; binding =
                       { id = Id.gen ()
                       ; scope = State.var_scope C.state
                       ; name = Symbol.create (Label.get_name label)
                       ; span = field_span
                       ; ty = used_expr.data.ty
                       ; label
                       ; mut = false
                       }
                   }
               | Index _ | Expr _ ->
                 error span "must use named field";
                 P_Placeholder)
            | _ ->
              error span "Can't use this";
              P_Placeholder
          in
          let used_expr = E_Claim used_expr |> init_expr used_expr.data.span C.state in
          let pattern = pattern |> init_pattern used_expr.data.span C.state in
          const_let span pattern used_expr (module C)
        | _ ->
          error span "use .* must be expr";
          init_error span C.state kind)
  }
;;

let use_dot_star : core_syntax =
  { name = "use .*"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let used = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let span = ast.span in
        match kind with
        | Expr ->
          let scope = State.var_scope C.state in
          let used, used_expr =
            Compiler.eval ~ty:(Ty.new_not_inferred ~scope ~span:used.span) (module C) used
          in
          let bindings =
            match Value.ty_of used |> Ty.await_inferred with
            | T_Tuple { name = _; tuple } ->
              tuple.named
              |> StringMap.to_list
              |> List.filter_map (fun (name, field) ->
                let field : Types.ty_tuple_field = field in
                field.label
                |> Option.map (fun label ->
                  ({ id = Id.gen ()
                   ; scope
                   ; name =
                       (match field.symbol with
                        | Some symbol -> symbol
                        | None -> Symbol.create name)
                   ; span = Label.get_span label
                   ; ty = field.ty
                   ; label
                   ; mut = false
                   }
                   : binding)))
            | other ->
              error span "can't use .* %a" Ty.Shape.print other;
              []
          in
          bindings
          |> List.iter (fun binding ->
            C.state |> Compiler.inject_binding ~only_compiler:false binding);
          let result =
            E_UseDotStar
              { used =
                  const_shape used
                  |> init_expr used_expr.data.span C.state
                  |> Compiler.data_add Expr (used_expr, used) kind
              ; bindings
              }
            |> init_expr span C.state
          in
          ignore @@ Interpreter.eval C.state.interpreter result;
          result
        | _ ->
          error span "use .* must be expr";
          init_error span C.state kind)
  }
;;

let comptime : core_syntax =
  { name = "comptime"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let expr = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let span = ast.span in
        match kind with
        | Expr ->
          let expr = C.compile Expr expr in
          let value = eval_const ~async:true C.state expr in
          const_shape value
          |> init_expr span C.state
          |> Compiler.data_add Expr (expr, value) kind
          |> Compiler.set_evaled value kind
        | _ ->
          error span "comptime must be expr";
          init_error span C.state kind)
  }
;;

let if' : core_syntax =
  { name = "if"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let cond, then_case, else_case =
          children
          |> Ast.flatten_children
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
          init_error span C.state kind)
  }
;;

let impl_syntax : core_syntax =
  { name = "impl syntax"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let name, impl =
            children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "name"; "impl" ]
          in
          (match name.shape with
           | Complex { rule = { name = "core:scope"; _ }; root = { children; _ } } ->
             let inner =
               children
               |> Tuple.unwrap_single_unnamed
               |> Ast.Child.expect_ast
               |> function
               | { shape = Complex inner; _ } -> inner
               | _ -> failwith __LOC__
             in
             let rule = inner.rule in
             let fields = inner.root.children |> Ast.flatten_children in
             let impl_expr =
               (* TODO maybe reduce copypasta here and compiling fn *)
               let ty : Types.ty_fn =
                 let scope = State.var_scope C.state in
                 { arg = Ty.new_not_inferred ~scope ~span:(Span.of_ocaml __POS__)
                 ; result = Ty.new_not_inferred ~scope ~span:(Span.of_ocaml __POS__)
                 }
               in
               let def : Types.maybe_compiled_fn =
                 { span = impl.span; compiled = None; on_compiled = [] }
               in
               State.Scope.fork (fun () ->
                 let state = State.enter_scope C.state ~span ~recursive:false in
                 let arg =
                   P_Tuple
                     { parts =
                         fields
                         |> Tuple.to_seq
                         |> Seq.map
                              (fun
                                  ((member : Tuple.member), (field : Ast.t))
                                   : pattern Types.tuple_part_of
                                 ->
                                 let field : pattern Types.tuple_field_of =
                                   { label_span = field.span
                                   ; label =
                                       (match member with
                                        | Index _ -> None
                                        | Name name ->
                                          Some (Label.create_definition field.span name))
                                   ; field = C.compile ~state Pattern field
                                   }
                                 in
                                 Field field)
                         |> List.of_seq
                     }
                   |> init_pattern name.span C.state
                 in
                 state |> Compiler.inject_pattern_bindings ~only_compiler:false arg;
                 let body = C.compile ~state Expr impl in
                 Compiler.finish_compiling def { arg; body };
                 ty.arg |> Inference.Ty.expect_inferred_as ~span:arg.data.span arg.data.ty;
                 ty.result
                 |> Inference.Ty.expect_inferred_as ~span:body.data.span body.data.ty);
               E_Fn { def; ty } |> init_expr span C.state
             in
             let impl = Interpreter.eval C.state.interpreter impl_expr in
             Hashtbl.add C.state.custom_syntax_impls rule.id impl;
             const_shape (V_Unit |> Value.inferred ~span)
             |> init_expr span C.state
             |> Compiler.data_add Expr (impl_expr, impl) kind
           | _ ->
             let name_value, name_expr =
               Compiler.eval ~ty:(Ty.inferred ~span:name.span T_String) (module C) name
             in
             let name = name_value |> Value.expect_string in
             let impl, impl_expr =
               Compiler.eval
                 ~ty:
                   ((* TODO *)
                    Ty.new_not_inferred
                      ~scope:(State.var_scope C.state)
                      ~span:impl.span)
                 (module C)
                 impl
             in
             (match name with
              | Some name ->
                Kast_default_syntax.ruleset
                |> Kast_parser.Ruleset.find_rule_opt name
                |> (function
                 | Some rule -> Hashtbl.add C.state.custom_syntax_impls rule.id impl
                 | None -> Error.error span "Syntax rule not found: %S" name)
              | None -> Error.error name_expr.data.span "Name must be a string");
             const_shape (V_Unit |> Value.inferred ~span)
             |> init_expr span C.state
             |> Compiler.data_add Expr (name_expr, name_value) kind
             |> Compiler.data_add Expr (impl_expr, impl) kind)
        | TyExpr ->
          error span "impl syntax can't be assignee";
          init_error span C.state kind
        | Assignee ->
          error span "impl syntax can't be assignee";
          init_error span C.state kind
        | Pattern ->
          error span "impl syntax can't be assignee";
          init_error span C.state kind)
  }
;;

let field_init : core_syntax =
  { name = "field init"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        : a -> comma_impl (module C) kind ast)
  }
;;

let quote : core_syntax =
  { name = "quote"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let body = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let rec construct (ast : Ast.t) : expr =
            match ast.shape with
            | Ast.Error _ | Ast.Simple _ | Ast.Empty ->
              const_shape (V_Ast ast |> Value.inferred ~span:ast.span)
              |> init_expr ast.span C.state
            | Ast.Complex { rule; root } when rule.name = "core:unquote" ->
              let unquote =
                root.children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast
              in
              C.compile kind unquote
            | Ast.Complex { rule; root } ->
              E_QuoteAst { rule; root = construct_group root }
              |> init_expr ast.span C.state
            | Ast.Syntax _ -> fail "TODO"
          and construct_group (group : Ast.group) : Expr.Shape.quote_ast_group =
            { rule = group.rule
            ; children =
                group.children
                |> Tuple.map (fun (child : Ast.child) : Expr.Shape.quote_ast_child ->
                  match child with
                  | Group child_group -> Group (construct_group child_group)
                  | Ast child -> Ast (construct child))
            ; span = group.span
            }
          in
          construct body
        | _ ->
          error span "quote must be expr";
          init_error span C.state kind)
  }
;;

let loop : core_syntax =
  { name = "loop"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let body = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | Expr ->
          E_Loop
            { body =
                C.compile
                  ~state:(State.enter_scope ~span C.state ~recursive:false)
                  Expr
                  body
            }
          |> init_expr span C.state
        | _ ->
          error span "loop must be expr";
          init_error span C.state kind)
  }
;;

let unwindable : core_syntax =
  { name = "unwindable"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let token, body =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "token"; "body" ]
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let token = C.compile Pattern token in
          let state = State.enter_scope ~span C.state ~recursive:false in
          state |> Compiler.inject_pattern_bindings ~only_compiler:false token;
          let body = C.compile ~state Expr body in
          E_Unwindable { token; body } |> init_expr span C.state
        | _ ->
          error span "unwindable must be expr";
          init_error span C.state kind)
  }
;;

let unwind : core_syntax =
  { name = "unwind"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let token, value =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "token"; "value" ]
        in
        match kind with
        | Expr ->
          E_Unwind { token = C.compile Expr token; value = C.compile Expr value }
          |> init_expr span C.state
        | _ ->
          error span "unwind must be expr";
          init_error span C.state kind)
  }
;;

let target_dependent : core_syntax =
  { name = "target_dependent"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let branches =
          children |> Ast.flatten_children |> Tuple.unwrap_single_named "branches"
        in
        let branches =
          Ast.collect_list
            ~binary_rule_name:"core:union"
            ~trailing_or_leading_rule_name:"core:leading union"
            branches
        in
        let branches =
          branches
          |> List.filter_map (fun (branch : Ast.t) ->
            match branch.shape with
            | Complex { rule = { name = "core:fn"; _ }; root; _ } ->
              let cond = root.children |> Tuple.get_named "arg" |> Ast.Child.expect_ast in
              let body =
                root.children |> Tuple.get_named "body" |> Ast.Child.expect_ast
              in
              let scope_with_target =
                C.state.scope
                |> State.Scope.inject_binding
                     ({ id = Id.gen ()
                      ; scope = None
                      ; name = Types.target_symbol
                      ; span
                      ; ty = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Target
                      ; label = Label.create_definition span Types.target_symbol.name
                      ; mut = false
                      }
                      : binding)
              in
              let state_with_target = { C.state with scope = scope_with_target } in
              Some
                ({ cond = C.compile ~state:state_with_target Expr cond
                 ; body =
                     C.compile
                       ~state:(C.state |> State.enter_scope ~span ~recursive:false)
                       Expr
                       body
                 }
                 : Types.expr_target_dependent_branch)
            | _ ->
              error branch.span "target dependent branch must use fn syntax";
              None)
        in
        match kind with
        | Expr ->
          E_TargetDependent
            { branches; captured = C.state.interpreter.scope; interpreter_branch = None }
          |> init_expr span C.state
        | _ ->
          error span "target dependent must be expr";
          init_error span C.state kind)
  }
;;

let match_ : core_syntax =
  { name = "match"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let value, branches =
            children
            |> Ast.flatten_children
            |> Tuple.unwrap_named2 [ "value"; "branches" ]
          in
          let value = C.compile PlaceExpr value in
          (* TODO reduce copypasta of this & target dependent *)
          let branches =
            Ast.collect_list
              ~binary_rule_name:"core:union"
              ~trailing_or_leading_rule_name:"core:leading union"
              branches
          in
          let branches =
            branches
            |> List.filter_map (fun (branch : Ast.t) ->
              match branch.shape with
              | Complex { rule = { name = "core:fn"; _ }; root; _ } ->
                let pattern =
                  root.children |> Tuple.get_named "arg" |> Ast.Child.expect_ast
                in
                let pattern = C.compile Pattern pattern in
                let body =
                  root.children |> Tuple.get_named "body" |> Ast.Child.expect_ast
                in
                let branch_state =
                  C.state |> State.enter_scope ~span:body.span ~recursive:false
                in
                Compiler.inject_pattern_bindings ~only_compiler:false pattern branch_state;
                let body = C.compile ~state:branch_state Expr body in
                Some ({ pattern; body } : Types.expr_match_branch)
              | _ ->
                error branch.span "match branch must use fn syntax";
                None)
          in
          E_Match { value; branches } |> init_expr span C.state
        | _ ->
          error span "match must be expr";
          init_error span C.state kind)
  }
;;

let inject_context : core_syntax =
  { name = "inject_context"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let context_type, value =
          children
          |> Ast.flatten_children
          |> Tuple.unwrap_named2 [ "context_type"; "value" ]
        in
        match kind with
        | Expr ->
          let context_ty_value, context_ty_expr =
            context_type
            |> Compiler.eval
                 ~ty:(Ty.inferred ~span:context_type.span T_ContextTy)
                 (module C)
          in
          (match context_ty_value |> Value.await_inferred with
           | V_ContextTy context_ty ->
             let value = C.compile Expr value in
             E_InjectContext { context_ty; value }
             |> init_expr span C.state
             |> Compiler.data_add Expr (context_ty_expr, context_ty_value) kind
           | _ -> init_error span C.state kind)
        | _ ->
          error span "inject_context must be expr";
          init_error span C.state kind)
  }
;;

let current_context : core_syntax =
  { name = "current_context"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let context_type =
          children |> Ast.flatten_children |> Tuple.unwrap_single_named "context_type"
        in
        match kind with
        | Expr ->
          let context_ty_value, context_ty_expr =
            context_type
            |> Compiler.eval
                 ~ty:(Ty.inferred ~span:context_type.span T_ContextTy)
                 (module C)
          in
          (match context_ty_value |> Value.await_inferred with
           | V_ContextTy context_ty ->
             E_CurrentContext { context_ty }
             |> init_expr span C.state
             |> Compiler.data_add Expr (context_ty_expr, context_ty_value) kind
           | _ -> init_error span C.state kind)
        | _ ->
          error span "current_context must be expr";
          init_error span C.state kind)
  }
;;

let binding : core_syntax =
  { name = "binding"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let binding = children |> Ast.flatten_children |> Tuple.unwrap_single_unnamed in
        let binding_value, binding_expr =
          binding
          |> Compiler.eval
               ~ty:(Ty.new_not_inferred ~scope:(State.var_scope C.state) ~span)
               (module C)
        in
        match binding_value.var |> Inference.Var.inferred_opt with
        | Some (V_Blocked { shape = BV_Binding binding; ty = _ }) ->
          let place =
            PE_Binding binding
            |> init_place_expr span C.state
            |> Compiler.data_add Expr (binding_expr, binding_value) PlaceExpr
          in
          (match kind with
           | PlaceExpr -> place
           | Expr -> E_Claim place |> init_expr span C.state
           | _ ->
             error span "binding must be expr (TODO)";
             init_error span C.state kind)
        | _ -> init_error span C.state kind)
  }
;;

let __file__ : core_syntax =
  { name = "__FILE__"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        : a ->
        let span = ast.span in
        match kind with
        | Expr ->
          const_shape (V_String (span.uri |> Uri.path) |> Value.inferred ~span)
          |> init_expr span C.state
        | _ ->
          error span "__FILE__ must be expr";
          init_error span C.state kind)
  }
;;

let current_compiler_scope : core_syntax =
  { name = "current_compiler_scope"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (_ : Ast.group)
        : a ->
        let span = ast.span in
        match kind with
        | Expr ->
          let scope = C.state.scope in
          const_shape (V_CompilerScope scope |> Value.inferred ~span)
          |> init_expr span C.state
        | _ ->
          error span "current_compiler_scope must be expr";
          init_error span C.state kind)
  }
;;

let variant_impl =
  fun (type a)
    (module C : Compiler.S)
    (kind : a compiled_kind)
    (ast : Ast.t)
    ({ children; _ } : Ast.group)
    : a ->
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
    E_Variant { label; label_span; value = value_ast |> Option.map (C.compile Expr) }
    |> init_expr span C.state
  | TyExpr ->
    (fun () ->
      let label = Label.create_definition label_ast.span label in
      TE_Variant
        { variants =
            [ { label_span; label; value = value_ast |> Option.map (C.compile TyExpr) } ]
        })
    |> init_ty_expr span C.state
  | Pattern ->
    let label = Label.create_reference label_ast.span label in
    P_Variant { label_span; label; value = value_ast |> Option.map (C.compile Pattern) }
    |> init_pattern span C.state
  | Assignee ->
    error span "variant can't be assignee";
    init_error span C.state kind
;;

let variant_without_value : core_syntax =
  { name = "variant_without_value"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        : a -> variant_impl (module C) kind ast group)
  }
;;

let union_impl
      (type a)
      (module C : Compiler.S)
      (kind : a compiled_kind)
      (ast : Ast.t)
      (_ : Ast.group)
  : a
  =
  let span = ast.span in
  let elements =
    Ast.collect_list
      ~binary_rule_name:"core:union"
      ~trailing_or_leading_rule_name:"core:leading union"
      ast
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
;;

let leading_union : core_syntax =
  { name = "leading union"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        : a -> union_impl (module C) kind ast group)
  }
;;

let union : core_syntax =
  { name = "union"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        : a -> union_impl (module C) kind ast group)
  }
;;

let variant : core_syntax =
  { name = "variant"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        (group : Ast.group)
        : a -> variant_impl (module C) kind ast group)
  }
;;

let and_ : core_syntax =
  { name = "and"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let lhs, rhs = children |> Ast.flatten_children |> Tuple.unwrap_unnamed2 in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          E_And { lhs = C.compile Expr lhs; rhs = C.compile Expr rhs }
          |> init_expr span C.state
        | _ ->
          error span "and must be expr";
          init_error span C.state kind)
  }
;;

let or_ : core_syntax =
  { name = "or"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let lhs, rhs = children |> Ast.flatten_children |> Tuple.unwrap_unnamed2 in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          E_Or { lhs = C.compile Expr lhs; rhs = C.compile Expr rhs }
          |> init_expr span C.state
        | _ ->
          error span "or must be expr";
          init_error span C.state kind)
  }
;;

let mut : core_syntax =
  { name = "mut"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let inner = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | Pattern -> C.compile ~state:{ C.state with mut_enabled = true } Pattern inner
        | Expr | PlaceExpr | TyExpr | Assignee ->
          error span "mut must be pattern";
          init_error span C.state kind)
  }
;;

let ref_impl ~name ~(mut : bool) : core_syntax =
  { name
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let inner = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          E_Ref { mut; place = C.compile PlaceExpr inner } |> init_expr span C.state
        | TyExpr ->
          (fun () ->
            TE_Ref
              { mut = IsMutable.new_inferred ~span mut
              ; referenced = C.compile TyExpr inner
              })
          |> init_ty_expr span C.state
        | Pattern -> P_Ref (C.compile Pattern inner) |> init_pattern span C.state
        | Assignee ->
          error span "ref can not be assignee";
          init_error span C.state kind)
  }
;;

let ref_ = ref_impl ~name:"ref" ~mut:false
let ref_mut = ref_impl ~name:"ref_mut" ~mut:true

let deref : core_syntax =
  { name = "deref"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let ref = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | PlaceExpr -> PE_Deref (C.compile Expr ref) |> init_place_expr span C.state
        | Expr -> E_Claim (C.compile PlaceExpr ast) |> init_expr span C.state
        | Assignee -> A_Place (C.compile PlaceExpr ast) |> init_assignee span C.state
        | _ ->
          error span "deref must be expr";
          init_error span C.state kind)
  }
;;

let impl_cast : core_syntax =
  { name = "impl_cast"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let value, target, impl =
          children
          |> Ast.flatten_children
          |> Tuple.unwrap_named3 [ "value"; "target"; "impl" ]
        in
        match kind with
        | Expr ->
          let target, target_expr =
            Compiler.eval
              ~ty:(Ty.new_not_inferred ~scope:(State.var_scope C.state) ~span:target.span)
              (module C)
              target
          in
          E_ImplCast { value = C.compile Expr value; target; impl = C.compile Expr impl }
          |> init_expr span C.state
          |> Compiler.data_add Expr (target_expr, target) kind
        | _ ->
          error span "impl cast must be expr";
          init_error span C.state kind)
  }
;;

let impl_as_module : core_syntax =
  { name = "impl_as_module"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let value, impl =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "value"; "impl" ]
        in
        match kind with
        | Expr ->
          let scope = State.var_scope C.state in
          let value, value_expr =
            Compiler.eval ~ty:(Ty.new_not_inferred ~scope ~span) (module C) value
          in
          Log.trace (fun log ->
            log "impl %a as_module at %a" Value.print value Span.print span);
          let impl_temp = Value.new_not_inferred ~scope ~span in
          Kast_interpreter.impl_cast_as_module
            ~span
            C.state.interpreter
            ~value
            ~impl:impl_temp;
          let impl, impl_expr =
            Compiler.eval ~ty:(Ty.new_not_inferred ~scope ~span) (module C) impl
          in
          Inference.Value.expect_inferred_as ~span impl impl_temp;
          const_shape (V_Unit |> Value.inferred ~span)
          |> init_expr span C.state
          |> Compiler.data_add Expr (value_expr, value) kind
          |> Compiler.data_add Expr (impl_expr, impl) kind
        | _ ->
          error span "impl cast must be expr";
          init_error span C.state kind)
  }
;;

let cast : core_syntax =
  { name = "cast"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let value, target =
          children |> Ast.flatten_children |> Tuple.unwrap_named2 [ "value"; "target" ]
        in
        match kind with
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | Expr ->
          let target, target_expr =
            Compiler.eval
              ~ty:(Ty.new_not_inferred ~scope:(State.var_scope C.state) ~span:target.span)
              (module C)
              target
          in
          E_Cast { value = C.compile Expr value; target }
          |> init_expr span C.state
          |> Compiler.data_add Expr (target_expr, target) kind
        | _ ->
          error span "cast must be expr";
          init_error span C.state kind)
  }
;;

let by_ref name ~mut : core_syntax =
  { name
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let inner = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        match kind with
        | Pattern ->
          C.compile ~state:{ C.state with bind_mode = ByRef { mut } } Pattern inner
        | _ ->
          error span "by_ref must be pattern";
          init_error span C.state kind)
  }
;;

let typeof : core_syntax =
  { name = "typeof"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let expr = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let expr () =
          let expr = C.compile Expr expr in
          let ty_value = V_Ty expr.data.ty |> Value.inferred ~span in
          const_shape ty_value
          |> init_expr span C.state
          |> Compiler.data_add Expr (expr, ty_value) Expr
        in
        match kind with
        | Expr -> expr ()
        | PlaceExpr -> Compiler.temp_expr (module C) ast
        | TyExpr -> (fun () -> TE_Expr (expr ())) |> init_ty_expr span C.state
        | Pattern | Assignee ->
          error span "typeof can't be pattern | assignee";
          init_error span C.state kind)
  }
;;

let include_ast : core_syntax =
  { name = "include_ast"
  ; handle =
      (fun (type a)
        (module C : Compiler.S)
        (kind : a compiled_kind)
        (ast : Ast.t)
        ({ children; _ } : Ast.group)
        : a ->
        let span = ast.span in
        let ast = children |> Tuple.unwrap_single_unnamed |> Ast.Child.expect_ast in
        let value, expr = Compiler.eval ~ty:(T_Ast |> Ty.inferred ~span) (module C) ast in
        match value |> Value.expect_ast with
        | Some ast -> C.compile kind ast |> Compiler.data_add Expr (expr, value) kind
        | None -> init_error span C.state kind)
  }
;;

let core =
  [ apply
  ; instantiate_generic
  ; then'
  ; stmt
  ; scope
  ; assign
  ; let'
  ; placeholder
  ; fn_type
  ; fn
  ; generic
  ; unit
  ; type'
  ; type_expr
  ; newtype
  ; type_ascribe
  ; import
  ; include'
  ; const
  ; native
  ; module'
  ; dot
  ; comma
  ; trailing_comma
  ; use
  ; use_dot_star
  ; comptime
  ; true'
  ; false'
  ; if'
  ; impl_syntax
  ; field_init
  ; quote
  ; loop
  ; unwindable
  ; unwind
  ; target_dependent
  ; match_
  ; inject_context
  ; current_context
  ; binding
  ; __file__
  ; current_compiler_scope
  ; variant
  ; variant_without_value
  ; leading_union
  ; union
  ; and_
  ; or_
  ; ref_
  ; ref_mut
  ; mut
  ; deref
  ; impl_cast
  ; impl_as_module
  ; cast
  ; by_ref "by_ref" ~mut:false
  ; by_ref "by_ref_mut" ~mut:true
  ; typeof
  ; include_ast
  ]
;;

let all : core_syntax StringMap.t =
  core |> List.map (fun handler -> handler.name, handler) |> StringMap.of_list
;;

let handle name (module C : Compiler.S) kind (ast : Ast.t) root =
  match all |> StringMap.find_opt name with
  | None ->
    error ast.span "there is no core syntax %S" name;
    init_error ast.span C.state kind
  | Some core_syntax -> core_syntax.handle (module C) kind ast root
;;
