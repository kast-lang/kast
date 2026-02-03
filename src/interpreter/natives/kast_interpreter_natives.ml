open Std
open Kast_util
open Kast_types
module Inference = Kast_inference
open Kast_interpreter_core
open Common

type natives = Types.natives
type t = natives

exception Panic of string

let init_natives () =
  let plain_types : (string * Ty.Shape.t) list =
    [ "Unit", T_Unit
    ; "Int32", T_Int32
    ; "Int64", T_Int64
    ; "Float64", T_Float64
    ; "String", T_String
    ; "Char", T_Char
    ; "Type", T_Ty
    ; "Bool", T_Bool
    ; "Ast", T_Ast
    ]
  in
  let generic_types : (string * (Ty.Shape.t -> Ty.Shape.t)) list =
    [ ("UnwindToken", fun result -> T_UnwindToken { result = Ty.inferred ~span result }) ]
  in
  let types =
    (plain_types
     |> List.map (fun (name, ty) : (string * (ty -> value)) ->
       name, fun _ -> V_Ty (Ty.inferred ~span ty) |> Value.inferred ~span))
    @ (generic_types
       |> List.map (fun (name, f) : (string * (ty -> value)) ->
         let impl =
           fun ~caller ~state:_ arg ->
           with_return (fun { return } : value ->
             let error () =
               return (V_Ty (Ty.inferred ~span T_Error) |> Value.inferred ~span)
             in
             let arg = single_arg ~span arg in
             let arg =
               arg
               |> Value.expect_ty
               |> Option.unwrap_or_else (fun () ->
                 Error.error caller "%a expected a type arg" String.print_debug name;
                 error ())
               |> fun ty -> Ty.await_inferred ty
             in
             V_Ty (Ty.inferred ~span (f arg)) |> Value.inferred ~span)
         in
         ( name
         , fun ty ->
             let fn_ty : Types.ty_fn =
               { args =
                   { ty =
                       Ty.inferred ~span
                       <| T_Tuple
                            { name = OptionalName.new_inferred ~span None
                            ; tuple =
                                Tuple.make
                                  [ ({ ty = Ty.inferred ~span T_Ty
                                     ; symbol = None
                                     ; label = None
                                     }
                                     : Types.ty_tuple_field)
                                  ]
                                  []
                            }
                   }
               ; result = Ty.inferred ~span T_Ty
               }
             in
             ty |> Inference.Ty.expect_inferred_as ~span (T_Fn fn_ty |> Ty.inferred ~span);
             V_NativeFn { id = Id.gen (); name; ty = fn_ty; impl } |> Value.inferred ~span
         )))
  in
  let natives : natives =
    let list : (string * (ty -> value)) list =
      [ native_fn "create_context_type" (fun _ty ~caller ~state:_ arg ->
          let arg = single_arg ~span arg in
          match arg |> Value.await_inferred with
          | V_Ty ty -> V_ContextTy { id = Id.gen (); ty } |> Value.inferred ~span
          | _ ->
            Error.error caller "create_context_type expected a type";
            V_Error |> Value.inferred ~span)
      ; native_fn "panic" (fun _ty ~caller:_ ~state:_ arg ->
          let arg = single_arg ~span arg in
          match arg |> Value.expect_string with
          | Some s -> raise (Panic s)
          | None -> V_Error |> Value.inferred ~span)
      ; native_fn "new_opaque_type" (fun _ty ~caller:_ ~state _arg ->
          V_Ty
            (Ty.inferred
               ~span
               (T_Opaque { name = current_name state |> Name.new_inferred ~span }))
          |> Value.inferred ~span)
      ; native_fn "@parse" (fun _ty ~caller ~state:_ arg ->
          let arg = single_arg ~span arg in
          match arg |> Value.expect_string with
          | Some contents ->
            let source : Source.t = { contents; uri = Uri.fake "@parse" } in
            (* TODO actually using current syntax? *)
            let { ast; trailing_comments = _; eof = _; ruleset_with_all_new_syntax = _ }
              : Kast_parser.result
              =
              Kast_parser.parse source Kast_default_syntax.ruleset
            in
            V_Ast (Kast_ast_init.init_ast ast) |> Value.inferred ~span
          | None ->
            Error.error caller "@parse needs a String as arg";
            V_Error |> Value.inferred ~span)
      ]
      @ types
      @ Mod_fs.init ()
      @ Mod_sys.init ()
      @ Mod_random.init ()
      @ Mod_char.init ()
      @ Mod_string.init ()
      @ Mod_dbg.init ()
      @ Mod_net.init ()
      @ Mod_io.init ()
      @ Mod_cmp.init ()
      @ Mod_op.init ()
      @ Mod_reflection.init ()
      @ Mod_syntax.init ()
    in
    { by_name = list |> StringMap.of_list }
  in
  natives
;;

module Fs = Mod_fs
module Sys = Mod_sys
module Random = Mod_random
module Char = Mod_char
module String = Mod_string
module Dbg = Mod_dbg
module Net = Mod_net
module Io = Mod_io
module Cmp = Mod_cmp
module Op = Mod_op
module Reflection = Mod_reflection
module Syntax = Mod_syntax
