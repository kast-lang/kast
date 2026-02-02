open Std
open Kast_util
module Effect = Compiler_types.CompilerEffect
open Kast_types
module Token = Kast_token
module Interpreter = Kast_interpreter
open Init
module Error = Error
module Scope = State.Scope

let init = State.init

type state = State.t
type import_cache = State.import_cache

let const_shape = Types.const_shape
let init_import_cache = State.init_import_cache
let get_data = Compiler.get_data

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
  fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; data = { span; hygiene; def_site } } : Ast.t = ast in
  Kast_profiling.record
    (fun () -> make_string "Compiling %a" Span.print span)
    (fun () ->
       try
         state |> State.enter_ast_def_site ast;
         Log.trace (fun log ->
           log
             "compiling with def_scope=%a at %a"
             Id.print
             (state.scopes |> State.Scopes.def_site).id
             Span.print
             span);
         match ast.shape with
         | Ast.Empty ->
           let expr () =
             const_shape (V_Unit |> Value.inferred ~span) |> init_expr span state
           in
           let result : a =
             match kind with
             | Assignee -> A_Unit |> init_assignee span state
             | Expr -> expr ()
             | PlaceExpr -> PE_Temp (expr ()) |> init_place_expr span state
             | TyExpr -> (fun () -> TE_Unit) |> init_ty_expr span state
             | Pattern -> P_Unit |> init_pattern span state
           in
           result
         | Ast.Error _ ->
           Error.error span "Trying to compile error node";
           init_error span state kind
         | Ast.Simple { token; _ } ->
           (match kind with
            | PlaceExpr ->
              (match token.shape with
               | Token.Shape.Ident ident ->
                 let local =
                   State.Scopes.find
                     ~hygiene
                     ~from_scope:(State.var_scope state)
                     ~from:span
                     ident.name
                     state.scopes
                 in
                 local |> Compiler.local_place_expr def_site.interpreter span state
               | _ -> PE_Temp (compile state Expr ast) |> init_place_expr span state)
            | Expr ->
              (match token.shape with
               | Token.Shape.Ident ident ->
                 let local =
                   State.Scopes.find
                     ~hygiene
                     ~from_scope:(State.var_scope state)
                     ~from:span
                     ident.name
                     state.scopes
                 in
                 local |> Compiler.local_expr def_site.interpreter span state
               | Token.Shape.String s ->
                 let value : Value.shape =
                   match s.delimeter with
                   | "\"" -> V_String s.contents
                   | "'" ->
                     (match String.into_single_utf8 s.contents with
                      | Some c -> V_Char c
                      | None ->
                        Error.error span "Char literals must have a single char";
                        V_Error)
                   | _ ->
                     Error.error
                       span
                       "Unexpected delimeter %a"
                       String.print_debug
                       s.delimeter;
                     V_Error
                 in
                 const_shape (value |> Value.inferred ~span) |> init_expr span state
               | Token.Shape.Number { raw; _ } ->
                 let default : Ty.Shape.t =
                   if String.contains raw '.' then T_Float64 else T_Int32
                 in
                 let scope = State.var_scope state in
                 let ty = Ty.new_not_inferred ~scope ~span in
                 let setup_default () =
                   ty |> Inference.Ty.expect_inferred_as ~span (Ty.inferred ~span default)
                 in
                 ty.var |> Inference.Var.setup_default setup_default;
                 let const = Value.new_not_inferred_of_ty ~scope ~span ty in
                 const.var |> Inference.Var.setup_default setup_default;
                 ty.var
                 |> Inference.Var.once_inferred (fun (ty : Ty.Shape.t) ->
                   let actual_const : Value.Shape.t =
                     match ty with
                     | T_Int32 ->
                       let value = Int32.of_string raw in
                       V_Int32 value
                     | T_Int64 ->
                       let value = Int64.of_string raw in
                       V_Int64 value
                     | T_Float64 ->
                       let value = Float.of_string raw in
                       V_Float64 value
                     | other ->
                       Error.error
                         span
                         "Number literal inferred as %a"
                         Ty.Shape.print
                         other;
                       V_Error
                   in
                   const
                   |> Inference.Value.expect_inferred_as
                        ~span
                        (actual_const |> Value.inferred ~span));
                 const_shape const |> init_expr span state
               | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                 unreachable "!")
            | TyExpr ->
              (fun () -> TE_Expr (compile state Expr ast)) |> init_ty_expr span state
            | Assignee ->
              (match token.shape with
               | Token.Shape.Ident _ ->
                 A_Place (compile state PlaceExpr ast) |> init_assignee span state
               | Token.Shape.String _ ->
                 Error.error span "string can't be assignee";
                 init_error span state kind
               | Token.Shape.Number _ ->
                 Error.error span "number can't be assignee";
                 init_error span state kind
               | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                 unreachable "!")
            | Pattern ->
              (match token.shape with
               | Token.Shape.Ident ident ->
                 let scope = State.var_scope state in
                 let binding : binding =
                   { id = Id.gen ()
                   ; scope
                   ; name = Symbol.create ident.name
                   ; ty = Ty.new_not_inferred ~scope ~span:token.span
                   ; span
                   ; label = Label.create_definition span ident.name
                   ; mut = state.mut_enabled
                   ; hygiene
                   ; def_site = state.scopes.def_site
                   }
                 in
                 P_Binding { bind_mode = state.bind_mode; binding }
                 |> init_pattern span state
               | Token.Shape.String _ ->
                 Error.error span "string can't be pattern";
                 init_error span state kind
               | Token.Shape.Number _ ->
                 Error.error span "number can't be pattern";
                 init_error span state kind
               | Token.Shape.Comment _ | Token.Shape.Punct _ | Token.Shape.Eof ->
                 unreachable "!"))
         | Ast.Complex { rule; root } ->
           (match rule.name |> String.strip_prefix ~prefix:"core:" with
            | Some name -> Core_syntax.handle name (make_compiler state) kind ast root
            | None ->
              (match Hashtbl.find_opt state.custom_syntax_impls rule.id with
               | Some impl ->
                 (* TODO *)
                 let args =
                   root.children
                   |> Ast.flatten_children
                   |> Tuple.mapi (fun member (ast : Ast.t) : Types.value_tuple_field ->
                     let ast =
                       Kast_ast_init.init_ast_def_site
                         { compiler = Some (state.scopes |> State.Scopes.def_site)
                         ; interpreter = None
                         }
                         ast
                     in
                     (* let ast =
                       { ast with
                         data =
                           { ast.data with
                             def_site =
                               ast.data.def_site
                               |> Option.or_
                                    (Some (state.scopes |> State.Scopes.def_site))
                           }
                       }
                     in *)
                     { place =
                         Place.init ~mut:Immutable (V_Ast ast |> Value.inferred ~span)
                     ; ty_field =
                         { ty = Ty.inferred ~span T_Ast
                         ; label =
                             (match member with
                              | Index _ -> None
                              | Name name -> Some (Label.create_reference span name))
                         ; symbol = None
                         }
                     ; span
                       (* TODO not sure if this is correct span, but there is no span? *)
                     })
                 in
                 let arg : value =
                   V_Tuple
                     { ty =
                         { name =
                             OptionalName.new_not_inferred
                               ~scope:(State.var_scope state)
                               ~span
                         ; tuple =
                             args
                             |> Tuple.map (fun (field : Types.value_tuple_field) ->
                               field.ty_field)
                         }
                     ; tuple = args
                     }
                   |> Value.inferred ~span
                 in
                 let expr =
                   E_Apply
                     { f = const_shape impl |> init_expr span state
                     ; arg = const_shape arg |> init_expr span state
                     }
                   |> init_expr span state
                 in
                 let result = Interpreter.eval state.interpreter expr in
                 (match result.var |> Inference.Var.inferred_opt with
                  | Some (V_Ast result) -> compile state kind result
                  | _ ->
                    Error.error span "macro expanded not to ast???";
                    init_error span state kind)
               | None ->
                 Error.error
                   span
                   "Must impl rule before using it: %a"
                   String.print_debug
                   rule.name;
                 init_error span state kind))
         | Ast.Syntax { mode = _; value_after; comments_before = _; tokens = _ } ->
           (match value_after with
            | Some value -> compile state kind value
            | None ->
              (match kind with
               | Expr ->
                 const_shape (V_Unit |> Value.inferred ~span) |> init_expr span state
               | _ ->
                 Error.error span "expected a value after syntax";
                 init_error span state kind))
       with
       | Cancel -> raise Cancel
       | exc ->
         Log.trace (fun log ->
           log "Exception: %a" String.print_debug (Printexc.to_string exc));
         Log.error (fun log ->
           log
             "While compiling %a %a at %a"
             Compiler.CompiledKind.print
             kind
             Ast.Shape.print_short
             ast.shape
             Span.print
             span);
         raise exc)

and make_compiler (original_state : state) : (module Compiler.S) =
  (module struct
    let state = original_state

    let compile (type b) ?state (kind : b compiled_kind) (ast : Ast.t) : b =
      compile (state |> Option.value ~default:original_state) kind ast
    ;;
  end : Compiler.S)
;;

let rec default name_part ?(import_cache : import_cache option) () : state =
  let import_cache =
    import_cache |> Option.unwrap_or_else (fun () -> State.init_import_cache ())
  in
  let state = init ~import_cache ~compile_for:(Interpreter.default name_part) in
  let prelude = [%include_file "prelude.ks"] in
  let prelude_parsed =
    try
      Kast_parser.parse
        { contents = prelude; uri = Uri.fake "prelude" }
        Kast_default_syntax.ruleset
    with
    | effect Kast_parser.Import _, k -> Std.Effect.continue k Kast_parser.Ruleset.empty
  in
  let prelude_span = Span.of_ocaml __POS__ in
  let _prelude_value, _prelude_expr =
    Compiler.eval
      ~ty:(T_Unit |> Ty.inferred ~span:prelude_span)
      (make_compiler state)
      (prelude_parsed.ast |> Kast_ast_init.init_ast)
  in
  (State.default := fun name_part ~import_cache -> default name_part ~import_cache ());
  state
;;

let handle_parser_imports f state = Compiler.handle_parser_imports f (make_compiler state)

let compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
  fun state kind ast ->
  state
  |> handle_parser_imports (fun () ->
    Fun.protect
      (fun () ->
         state.currently_compiled_file <- Some ast.data.span.uri;
         let result = compile state kind ast in
         Log.trace (fun log -> log "Completing inference");
         Kast_inference_completion.complete_compiled kind result;
         result)
      ~finally:(fun () -> state.currently_compiled_file <- None))
;;
