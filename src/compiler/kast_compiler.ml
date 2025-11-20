open Std
open Kast_util
module Effect = Compiler_types.CompilerEffect
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
module Ast = Kast_ast
open Init
module Error = Error
module Scope = State.Scope

type state = State.t
type import_cache = State.import_cache

let init_import_cache = State.init_import_cache

(* TODO compile_for - figure out *)
let init : import_cache:import_cache -> compile_for:Interpreter.state -> state =
 fun ~import_cache ~compile_for ->
  let scope = State.Scope.init ~recursive:false in
  let scope =
    SymbolMap.fold
      (fun name (local : Types.interpreter_local) scope ->
        scope
        |> State.Scope.inject_binding
             {
               id = Id.gen ();
               name;
               ty = Value.ty_of local.value;
               span = Span.fake "<interpreter>";
               label = local.ty_field.label;
             })
      compile_for.scope.locals.by_symbol scope
  in
  {
    scope;
    currently_compiled_file = None;
    interpreter = compile_for;
    import_cache;
    custom_syntax_impls = Hashtbl.create 0;
  }

type 'a compiled_kind = 'a Compiler_types.compiled_kind

let get_data = Compiler.get_data

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
 fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; span } : Ast.t = ast in
  try
    match ast.shape with
    | Ast.Error _ ->
        Error.error span "Trying to compile error node";
        init_error span state kind
    | Ast.Simple { token; _ } -> (
        match kind with
        | Expr -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                E_Binding
                  (State.Scope.find_binding ~from:ast.span ident.name
                     state.scope)
                |> init_expr span state
            | Token.Shape.String s ->
                let value : Value.shape =
                  match s.delimeter with
                  | "\"" -> V_String s.contents
                  | "'" ->
                      if s.contents |> String.length = 1 then
                        V_Char (String.get s.contents 0 |> Option.get)
                      else (
                        Error.error ast.span
                          "Char literals must have a single char";
                        V_Error)
                  | _ ->
                      Error.error ast.span "Unexpected delimeter %S" s.delimeter;
                      V_Error
                in
                E_Constant { shape = value } |> init_expr span state
            | Token.Shape.Number { raw; _ } ->
                let value = Int32.of_string raw in
                E_Constant { shape = V_Int32 value } |> init_expr span state
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | TyExpr -> TE_Expr (compile state Expr ast) |> init_ty_expr span state
        | Assignee -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                A_Binding
                  (State.Scope.find_binding ~from:ast.span ident.name
                     state.scope)
                |> init_assignee span state
            | Token.Shape.String _ ->
                Error.error span "string can't be assignee";
                init_error span state kind
            | Token.Shape.Number _ ->
                Error.error span "number can't be assignee";
                init_error span state kind
            | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                unreachable "!")
        | Pattern -> (
            match token.shape with
            | Token.Shape.Ident ident ->
                let binding : binding =
                  {
                    id = Id.gen ();
                    name = Symbol.create ident.name;
                    ty = Ty.new_not_inferred ();
                    span = ast.span;
                    label = Label.create_definition ast.span ident.name;
                  }
                in
                P_Binding binding |> init_pattern span state
            | Token.Shape.String _ ->
                Error.error span "string can't be pattern";
                init_error span state kind
            | Token.Shape.Number _ ->
                Error.error span "number can't be pattern";
                init_error span state kind
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
                  |> Tuple.map (fun ast : Types.value_tuple_field ->
                      {
                        value = { shape = V_Ast ast };
                        ty_field =
                          {
                            ty = Ty.inferred T_Ast;
                            label = Label.create_definition ast.span "<TODO>";
                          };
                        span =
                          ast.span
                          (* TODO not sure if this is correct span, but there is no span? *);
                      })
                in
                let arg : value = { shape = V_Tuple { tuple = args } } in
                let expr =
                  E_Apply
                    {
                      f = E_Constant impl |> init_expr span state;
                      arg = E_Constant arg |> init_expr span state;
                    }
                  |> init_expr span state
                in
                let result = Interpreter.eval state.interpreter expr in
                match result.shape with
                | V_Ast result -> compile state kind result
                | _ ->
                    Error.error span "macro expanded not to ast???";
                    init_error span state kind)
            | None ->
                Error.error span "Must impl rule before using it: %S" rule.name;
                init_error span state kind))
    | Ast.Syntax { mode; value_after; comments_before = _; tokens = _ } -> (
        match value_after with
        | Some value -> compile state kind value
        | None -> (
            match kind with
            | Expr -> E_Constant { shape = V_Unit } |> init_expr span state
            | _ ->
                Error.error span "expected a value after syntax";
                init_error span state kind))
  with exc ->
    Log.error (fun log ->
        log "While compiling %a %a at %a" Compiler.CompiledKind.print kind
          Ast.Shape.print_short ast.shape Span.print ast.span);
    raise exc

and make_compiler (original_state : state) : (module Compiler.S) =
  (module struct
    let state = original_state

    let compile (type b) ?state (kind : b compiled_kind) (ast : Ast.t) : b =
      compile (state |> Option.value ~default:original_state) kind ast
  end : Compiler.S)

let default ?(import_cache : import_cache option) () : state =
  let import_cache =
    import_cache |> Option.unwrap_or_else (fun () -> State.init_import_cache ())
  in
  let interpreter_without_std = Interpreter.default () in
  let bootstrap = init ~import_cache ~compile_for:interpreter_without_std in
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
  let std_label =
    Label.create_definition (Span.beginning_of std_uri) std_symbol.name
  in
  let interpreter_with_std =
    Interpreter.init
      {
        by_symbol =
          SymbolMap.singleton std_symbol
            ({
               value = std;
               ty_field = { ty = Value.ty_of std; label = std_label };
             }
              : Types.interpreter_local);
      }
  in
  let scope = State.Scope.init ~recursive:false in
  let scope =
    scope
    |> State.Scope.inject_binding
         {
           id = Id.gen ();
           name = std_symbol;
           ty = Value.ty_of std;
           span = Span.beginning_of std_uri;
           label = std_label;
         }
  in
  {
    scope;
    currently_compiled_file = None;
    interpreter = interpreter_with_std;
    import_cache;
    custom_syntax_impls = bootstrap.custom_syntax_impls;
  }

let compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
 fun state kind ast ->
  Fun.protect
    (fun () ->
      state.currently_compiled_file <- Some ast.span.uri;
      compile state kind ast)
    ~finally:(fun () -> state.currently_compiled_file <- None)
