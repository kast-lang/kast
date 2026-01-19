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
    [ "unit", T_Unit
    ; "int32", T_Int32
    ; "int64", T_Int64
    ; "float64", T_Float64
    ; "string", T_String
    ; "char", T_Char
    ; "type", T_Ty
    ; "bool", T_Bool
    ; "ast", T_Ast
    ]
  in
  let generic_types : (string * (Ty.Shape.t -> Ty.Shape.t)) list =
    [ ("unwind_token", fun result -> T_UnwindToken { result = Ty.inferred ~span result })
    ]
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
             let arg =
               arg
               |> Value.expect_ty
               |> Option.unwrap_or_else (fun () ->
                 Error.error caller "%S expected a type arg" name;
                 error ())
               |> fun ty -> Ty.await_inferred ty
             in
             V_Ty (Ty.inferred ~span (f arg)) |> Value.inferred ~span)
         in
         ( name
         , fun ty ->
             let fn_ty : Types.ty_fn =
               { arg = Ty.inferred ~span T_Ty; result = Ty.inferred ~span T_Ty }
             in
             ty |> Inference.Ty.expect_inferred_as ~span (T_Fn fn_ty |> Ty.inferred ~span);
             V_NativeFn { id = Id.gen (); name; ty = fn_ty; impl } |> Value.inferred ~span
         )))
  in
  let natives : natives =
    let list : (string * (ty -> value)) list =
      [ native_fn "create_context_type" (fun _ty ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Ty ty -> V_ContextTy { id = Id.gen (); ty } |> Value.inferred ~span
          | _ ->
            Error.error caller "create_context_type expected a type";
            V_Error |> Value.inferred ~span)
      ; native_fn "panic" (fun _ty ~caller:_ ~state:_ arg ->
          match arg |> Value.expect_string with
          | Some s -> raise (Panic s)
          | None -> V_Error |> Value.inferred ~span)
      ; native_fn "new_opaque_type" (fun _ty ~caller:_ ~state _arg ->
          V_Ty
            (Ty.inferred
               ~span
               (T_Opaque { name = current_name state |> Name.new_inferred ~span }))
          |> Value.inferred ~span)
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
