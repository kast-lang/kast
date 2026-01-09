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
              { id = Id.gen ()
              ; scope = Some compile_for.scope
              ; name
              ; ty = local.place.ty
              ; span = Span.fake "<interpreter>"
              ; label = local.ty_field.label |> Option.get
              ; mut = Place.Mut.to_bool local.place.mut
              })
      compile_for.scope.locals.by_symbol
      scope
  in
  { scope
  ; currently_compiled_file = None
  ; interpreter = compile_for
  ; import_cache
  ; custom_syntax_impls = Hashtbl.create 0
  ; mut_enabled = false
  ; bind_mode = Claim
  }
;;

let get_data = Compiler.get_data

let rec compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
  fun (type a) (state : state) (kind : a compiled_kind) (ast : Ast.t) : a ->
  let { shape = _; span } : Ast.t = ast in
  Kast_profiling.record
    (fun () -> make_string "Compiling %a" Span.print ast.span)
    (fun () ->
       try
         match ast.shape with
         | Ast.Error _ ->
           Error.error span "Trying to compile error node";
           init_error span state kind
         | Ast.Simple { token; _ } ->
           (match kind with
            | PlaceExpr ->
              (match token.shape with
               | Token.Shape.Ident ident ->
                 PE_Binding
                   (State.Scope.find_binding
                      ~from_scope:(State.var_scope state)
                      ~from:ast.span
                      ident.name
                      state.scope)
                 |> init_place_expr span state
               | _ -> PE_Temp (compile state Expr ast) |> init_place_expr span state)
            | Expr ->
              (match token.shape with
               | Token.Shape.Ident _ ->
                 E_Claim (compile state PlaceExpr ast) |> init_expr span state
               | Token.Shape.String s ->
                 let value : Value.shape =
                   match s.delimeter with
                   | "\"" -> V_String s.contents
                   | "'" ->
                     if s.contents |> String.length = 1
                     then V_Char (String.get s.contents 0 |> Option.get)
                     else (
                       Error.error ast.span "Char literals must have a single char";
                       V_Error)
                   | _ ->
                     Error.error ast.span "Unexpected delimeter %S" s.delimeter;
                     V_Error
                 in
                 E_Constant (value |> Value.inferred ~span) |> init_expr span state
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
                 E_Constant const |> init_expr span state
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
                   ; span = ast.span
                   ; label = Label.create_definition ast.span ident.name
                   ; mut = state.mut_enabled
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
                   |> Tuple.map Ast.Child.expect_ast
                   |> Tuple.mapi (fun member (ast : Ast.t) : Types.value_tuple_field ->
                     { place =
                         Place.init
                           ~mut:Immutable
                           (V_Ast ast |> Value.inferred ~span:ast.span)
                     ; ty_field =
                         { ty = Ty.inferred ~span:ast.span T_Ast
                         ; label =
                             (match member with
                              | Index _ -> None
                              | Name name -> Some (Label.create_reference ast.span name))
                         }
                     ; span =
                         ast.span
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
                     { f = E_Constant impl |> init_expr span state
                     ; arg = E_Constant arg |> init_expr span state
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
                 Error.error span "Must impl rule before using it: %S" rule.name;
                 init_error span state kind))
         | Ast.Syntax { mode; value_after; comments_before = _; tokens = _ } ->
           (match value_after with
            | Some value -> compile state kind value
            | None ->
              (match kind with
               | Expr ->
                 E_Constant (V_Unit |> Value.inferred ~span) |> init_expr span state
               | _ ->
                 Error.error span "expected a value after syntax";
                 init_error span state kind))
       with
       | Cancel -> raise Cancel
       | exc ->
         Log.trace (fun log -> log "Exception: %S" (Printexc.to_string exc));
         Log.error (fun log ->
           log
             "While compiling %a %a at %a"
             Compiler.CompiledKind.print
             kind
             Ast.Shape.print_short
             ast.shape
             Span.print
             ast.span);
         raise exc)

and make_compiler (original_state : state) : (module Compiler.S) =
  (module struct
    let state = original_state

    let compile (type b) ?state (kind : b compiled_kind) (ast : Ast.t) : b =
      compile (state |> Option.value ~default:original_state) kind ast
    ;;
  end : Compiler.S)
;;

let default name_part ?(import_cache : import_cache option) () : state =
  let import_cache =
    import_cache |> Option.unwrap_or_else (fun () -> State.init_import_cache ())
  in
  let interpreter_without_std = Interpreter.default (Str "std") in
  let bootstrap = init ~import_cache ~compile_for:interpreter_without_std in
  let std_uri =
    Uri.append_if_relative
      (Stdlib.Effect.perform Effect.FindStd)
      (Uri.of_string "./lib.ks")
  in
  let std =
    Compiler.import ~span:(Span.fake "std-bootstrap") (make_compiler bootstrap) std_uri
  in
  let std_symbol = Symbol.create "std" in
  let std_label = Label.create_definition (Span.beginning_of std_uri) std_symbol.name in
  let interpreter_with_std =
    Interpreter.init
      name_part
      { by_symbol =
          SymbolMap.singleton
            std_symbol
            ({ place = Place.init ~mut:Immutable std
             ; ty_field = { ty = Value.ty_of std; label = Some std_label }
             }
             : Types.interpreter_local)
      }
  in
  (* TODO hack??? *)
  interpreter_with_std.cast_impls.map <- interpreter_without_std.cast_impls.map;
  interpreter_with_std.cast_impls.as_module
  <- interpreter_without_std.cast_impls.as_module;
  let scope = State.Scope.init ~recursive:false in
  let scope =
    scope
    |> State.Scope.inject_binding
         { id = Id.gen ()
         ; scope = None
         ; name = std_symbol
         ; ty = Value.ty_of std
         ; span = Span.beginning_of std_uri
         ; label = std_label
         ; mut = false
         }
  in
  { scope
  ; currently_compiled_file = None
  ; interpreter = interpreter_with_std
  ; import_cache
  ; custom_syntax_impls = bootstrap.custom_syntax_impls
  ; mut_enabled = false
  ; bind_mode = Claim
  }
;;

let compile : 'a. state -> 'a compiled_kind -> Ast.t -> 'a =
  fun state kind ast ->
  Fun.protect
    (fun () ->
       state.currently_compiled_file <- Some ast.span.uri;
       compile state kind ast)
    ~finally:(fun () -> state.currently_compiled_file <- None)
;;
