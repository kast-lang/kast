open Std
open Kast_util
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Init
module Error = Error

type state = State.t

module Effect = Compiler.Effect

(* TODO compile_for - figure out *)
let init : compile_for:Interpreter.state -> state =
 fun ~compile_for ->
  let scope = State.Scope.init () in
  let scope =
    SymbolMap.fold
      (fun name value scope ->
        scope
        |> State.Scope.inject_binding
             {
               name;
               ty = Value.ty_of value;
               span = Span.fake "<interpreter>";
               references = [];
             })
      compile_for.scope.locals.by_symbol scope
  in
  {
    scope;
    interpreter = compile_for;
    imported = State.init_imported ();
    custom_syntax_impls = Hashtbl.create 0;
  }

type 'a compiled_kind = 'a Compiler.compiled_kind

let get_data = Compiler.get_data

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
 fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; span } : Ast.t = ast in
  try
    match ast.shape with
    | Ast.Error _ ->
        Error.error span "Trying to compile error node";
        init_error span kind
    | Ast.Simple { token; _ } -> (
        match kind with
        | Expr -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                E_Binding
                  (State.Scope.find_binding ~from:ast.span ident.name
                     state.scope)
                |> init_expr span
            | Token.Shape.String s ->
                E_Constant { shape = V_String s.contents } |> init_expr span
            | Token.Shape.Number { raw; _ } ->
                let value = Int32.of_string raw in
                E_Constant { shape = V_Int32 value } |> init_expr span
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | TyExpr -> TE_Expr (compile state Expr ast) |> init_ty_expr span
        | Assignee -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                A_Binding
                  (State.Scope.find_binding ~from:ast.span ident.name
                     state.scope)
                |> init_assignee span
            | Token.Shape.String _ ->
                Error.error span "string can't be assignee";
                init_error span kind
            | Token.Shape.Number _ ->
                Error.error span "number can't be assignee";
                init_error span kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | Pattern -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                let binding : binding =
                  {
                    name = Symbol.create ident.name;
                    ty = Ty.new_not_inferred ();
                    span = ast.span;
                    references = [];
                  }
                in
                P_Binding binding |> init_pattern span
            | Token.Shape.String _ ->
                Error.error span "string can't be pattern";
                init_error span kind
            | Token.Shape.Number _ ->
                Error.error span "number can't be pattern";
                init_error span kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!"))
    | Ast.Complex { rule; root } -> (
        match rule.name |> String.strip_prefix ~prefix:"core:" with
        | Some name ->
            Core_syntax.handle name (make_compiler state) kind ast root
        | None -> (
            match Hashtbl.find_opt state.custom_syntax_impls rule.id with
            | Some impl -> (
                (* TODO *)
                let args =
                  root.children
                  |> Tuple.map Ast.Child.expect_ast
                  |> Tuple.map (fun ast : value -> { shape = V_Ast ast })
                in
                let arg : value = { shape = V_Tuple { tuple = args } } in
                let expr =
                  E_Apply
                    {
                      f = E_Constant impl |> init_expr span;
                      arg = E_Constant arg |> init_expr span;
                    }
                  |> init_expr span
                in
                let result = Interpreter.eval state.interpreter expr in
                match result.shape with
                | V_Ast result -> compile state kind result
                | _ ->
                    Error.error span "macro expanded not to ast???";
                    init_error span kind)
            | None ->
                Error.error span "Must impl rule before using it: %S" rule.name;
                init_error span kind))
    | Ast.Syntax { mode; value_after; comments_before = _; tokens = _ } -> (
        match value_after with
        | Some value -> compile state kind value
        | None -> (
            match kind with
            | Expr -> E_Constant { shape = V_Unit } |> init_expr span
            | _ ->
                Error.error span "expected a value after syntax";
                init_error span kind))
  with exc ->
    Log.error "While compiling %a %a at %a" Compiler.CompiledKind.print kind
      Ast.Shape.print_short ast.shape Span.print ast.span;
    raise exc

and make_compiler (original_state : state) : (module Compiler.S) =
  (module struct
    let state = original_state

    let compile (type b) ?state (kind : b compiled_kind) (ast : Ast.t) : b =
      compile (state |> Option.value ~default:original_state) kind ast
  end : Compiler.S)

let default () : state =
  let interpreter_without_std = Interpreter.default () in
  let bootstrap = init ~compile_for:interpreter_without_std in
  let std_uri =
    Uri.append_if_relative
      (Stdlib.Effect.perform Effect.FindStd)
      (Uri.of_string "./lib.ks")
  in
  let std =
    Compiler.import
      ~span:(Span.fake "std-bootstrap")
      (make_compiler bootstrap) std_uri
  in
  let std_symbol = Symbol.create "std" in
  let interpreter_with_std =
    Interpreter.init { by_symbol = SymbolMap.singleton std_symbol std }
  in
  let scope = State.Scope.init () in
  let scope =
    scope
    |> State.Scope.inject_binding
         {
           name = std_symbol;
           ty = Value.ty_of std;
           span = Span.beginning_of std_uri;
           references = [];
         }
  in
  {
    scope;
    interpreter = interpreter_with_std;
    imported = State.init_imported ();
    custom_syntax_impls = bootstrap.custom_syntax_impls;
  }
