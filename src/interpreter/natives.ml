open Std
open Kast_util
open Kast_types
module Inference = Kast_inference

type natives = Types.natives
type t = natives
type _ Effect.t += Input : string -> string Effect.t

exception Panic of string

let argv : string array ref = ref [||]

let init_natives () =
  let span = Span.of_ocaml __POS__ in

  let plain_types : (string * Ty.Shape.t) list =
    [
      ("unit", T_Unit);
      ("int32", T_Int32);
      ("int64", T_Int64);
      ("float64", T_Float64);
      ("string", T_String);
      ("char", T_Char);
      ("type", T_Ty);
      ("bool", T_Bool);
    ]
  in

  let generic_types : (string * (Ty.Shape.t -> Ty.Shape.t)) list =
    [
      ( "unwind_token",
        fun result -> T_UnwindToken { result = Ty.inferred ~span result } );
    ]
  in

  let types =
    (plain_types
    |> List.map (fun (name, ty) : (string * (ty -> value)) ->
        (name, fun _ -> V_Ty (Ty.inferred ~span ty) |> Value.inferred ~span)))
    @ (generic_types
      |> List.map (fun (name, f) : (string * (ty -> value)) ->
          let impl =
           fun ~caller ~state:_ arg ->
            with_return (fun { return } : value ->
                let error () =
                  return
                    (V_Ty (Ty.inferred ~span T_Error) |> Value.inferred ~span)
                in
                let arg =
                  arg |> Value.expect_ty
                  |> Option.unwrap_or_else (fun () ->
                      Error.error caller "%S expected a type arg" name;
                      error ())
                  |> fun ty -> Ty.await_inferred ty
                in

                V_Ty (Ty.inferred ~span (f arg)) |> Value.inferred ~span)
          in

          ( name,
            fun ty ->
              let fn_ty : Types.ty_fn =
                {
                  arg = Ty.inferred ~span T_Ty;
                  result = Ty.inferred ~span T_Ty;
                }
              in
              ty
              |> Inference.Ty.expect_inferred_as ~span
                   (T_Fn fn_ty |> Ty.inferred ~span);
              V_NativeFn { id = Id.gen (); name; ty = fn_ty; impl }
              |> Value.inferred ~span )))
  in

  let native_fn name impl : string * (ty -> value) =
    ( name,
      fun ty ->
        let fn_ty : Types.ty_fn =
          {
            arg = Ty.new_not_inferred ~span;
            result = Ty.new_not_inferred ~span;
          }
        in
        ty
        |> Inference.Ty.expect_inferred_as ~span
             (T_Fn fn_ty |> Ty.inferred ~span);
        V_NativeFn { id = Id.gen (); ty = fn_ty; name; impl = impl fn_ty }
        |> Value.inferred ~span )
  in

  let dbg =
    [
      (* TODO dbg should be polymorphic *)
      native_fn "dbg.print" (fun _ty ~caller:_ ~state:_ arg : value ->
          println "%a" Value.print arg;
          V_Unit |> Value.inferred ~span);
    ]
  in

  let mod_char =
    let code =
      native_fn "char.code" (fun _ty ~caller ~state:_ arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "char.code: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let c =
                arg |> Value.expect_char
                |> Option.unwrap_or_else (error "arg must be char")
              in
              V_Int32 (Char.code c |> Int32.of_int) |> Value.inferred ~span))
    in
    let from_code =
      native_fn "char.from_code" (fun _ty ~caller ~state:_ arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "char.from_code: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let code =
                arg |> Value.expect_int32
                |> Option.unwrap_or_else
                     (error
                        (make_string "arg must be uint32, got %a" Value.print
                           arg))
              in
              V_Char (Char.chr (Int32.to_int code)) |> Value.inferred ~span))
    in
    [ code; from_code ]
  in

  let mod_string =
    let at =
      native_fn "string.at" (fun _ty ~caller ~state:_ arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "string.at: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let arg =
                arg |> Value.expect_tuple
                |> Option.unwrap_or_else (error "arg must be tuple")
              in
              if not (arg.tuple |> Tuple.is_unnamed 2) then
                error "expected 2 unnamed fields" ();
              let s, idx = arg.tuple |> Tuple.unwrap_unnamed2 in
              let s =
                s.place |> Common.claim ~span:caller |> Value.expect_string
                |> Option.unwrap_or_else (error "expected string as first arg")
              in
              let idx =
                idx.place |> Common.claim ~span:caller |> Value.expect_int32
                |> Option.unwrap_or_else (error "expected idx be int32")
              in
              V_Char
                (String.get s (Int32.to_int idx)
                |> Option.unwrap_or_else (error "oob"))
              |> Value.inferred ~span))
    in
    let length =
      native_fn "string.length" (fun _ty ~caller ~state:_ arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "string.length: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let s =
                arg |> Value.expect_string
                |> Option.unwrap_or_else (error "expected string as arg")
              in
              V_Int32 (Int32.of_int (String.length s)) |> Value.inferred ~span))
    in
    let substring =
      native_fn "string.substring" (fun _ty ~caller ~state:_ arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "string.substring: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let arg =
                arg |> Value.expect_tuple
                |> Option.unwrap_or_else (error "arg must be tuple")
              in
              if not (arg.tuple |> Tuple.is_unnamed 3) then
                error "expected 2 unnamed fields" ();
              let s, start, len = arg.tuple |> Tuple.unwrap_unnamed3 in
              let s =
                s.place |> Common.claim ~span:caller |> Value.expect_string
                |> Option.unwrap_or_else (error "expected string as first arg")
              in
              let start =
                start.place |> Common.claim ~span:caller |> Value.expect_int32
                |> Option.unwrap_or_else (error "expected start be int32")
              in
              let len =
                len.place |> Common.claim ~span:caller |> Value.expect_int32
                |> Option.unwrap_or_else (error "expected len be int32")
              in
              V_String (String.sub s (Int32.to_int start) (Int32.to_int len))
              |> Value.inferred ~span))
    in
    let iter =
      native_fn "string.iter" (fun _ty ~caller ~state arg : value ->
          with_return (fun { return } ->
              let error msg () =
                Error.error caller "string.iter: %s" msg;
                return (V_Error |> Value.inferred ~span)
              in
              let arg =
                arg |> Value.expect_tuple
                |> Option.unwrap_or_else (error "arg must be tuple")
              in
              if not (arg.tuple |> Tuple.is_unnamed 2) then
                error "expected 2 unnamed fields" ();
              let s, f = arg.tuple |> Tuple.unwrap_unnamed2 in
              let s =
                s.place |> Common.claim ~span:caller |> Value.expect_string
                |> Option.unwrap_or_else (error "expected string as first arg")
              in
              let f = f.place |> Common.claim ~span:caller in
              let _ =
                f |> Value.expect_fn
                |> Option.unwrap_or_else (error "expected fn as second arg")
              in
              s
              |> String.iter (fun c ->
                  let c : value = V_Char c |> Value.inferred ~span in
                  ignore <| Common.call caller state f c;
                  ());
              V_Unit |> Value.inferred ~span))
    in
    [ at; length; substring; iter ]
  in

  let rng =
    [
      native_fn "rng.gen_range" (fun (ty : Types.ty_fn) ~caller ~state:_ arg ->
          (try
             let { ty = _; tuple } : Kast_types.Types.value_tuple =
               arg |> Value.expect_tuple |> Option.get
             in
             let min, max = tuple |> Tuple.unwrap_named2 [ "min"; "max" ] in
             let min = min.place |> Common.claim ~span:caller in
             let max = max.place |> Common.claim ~span:caller in
             match ty.result |> Ty.await_inferred with
             | T_Int32 ->
                 let min = min |> Value.expect_int32 |> Option.get in
                 let max = max |> Value.expect_int32 |> Option.get in
                 V_Int32 (Random.int32_in_range ~min ~max)
             | T_Int64 ->
                 let min = min |> Value.expect_int64 |> Option.get in
                 let max = max |> Value.expect_int64 |> Option.get in
                 V_Int64 (Random.int64_in_range ~min ~max)
             | T_Float64 ->
                 let min = min |> Value.expect_float64 |> Option.get in
                 let max = max |> Value.expect_float64 |> Option.get in
                 V_Float64 (min +. Random.float (max -. min))
             | _ ->
                 Error.error caller "idk how to generate %a" Ty.print ty.result;
                 V_Error
           with
            | Cancel -> raise Cancel
            | exc ->
                Error.error caller "rng.gen_range: %s" (Printexc.to_string exc);
                V_Error)
          |> Value.inferred ~span);
    ]
  in

  let sys =
    let chdir =
      native_fn "sys.chdir" (fun _ty ~caller ~state:_ arg : value ->
          match arg |> Value.await_inferred with
          | V_String path ->
              Sys.chdir path;
              V_Unit |> Value.inferred ~span
          | _ ->
              Error.error caller "sys.chdir expected string arg";
              V_Error |> Value.inferred ~span)
    in
    let argc =
      native_fn "sys.argc" (fun _ty ~caller:_ ~state:_ _arg : value ->
          V_Int32 (!argv |> Array.length |> Int32.of_int)
          |> Value.inferred ~span)
    in
    let argv_at =
      native_fn "sys.argv_at" (fun _ty ~caller ~state:_ arg : value ->
          match arg |> Value.await_inferred with
          | V_Int32 idx -> (
              match Array.get_opt !argv (Int32.to_int idx) with
              | None ->
                  Error.error caller "sys.argv_at out of bounds";
                  V_Error |> Value.inferred ~span
              | Some arg -> V_String arg |> Value.inferred ~span)
          | _ ->
              Error.error caller "sys.argv_at expected int32 arg";
              V_Error |> Value.inferred ~span)
    in
    let exec =
      native_fn "sys.exec" (fun _ty ~caller ~state:_ arg : value ->
          match arg |> Value.await_inferred with
          | V_String cmd ->
              V_Int32 (Sys.command cmd |> Int32.of_int) |> Value.inferred ~span
          | _ ->
              Error.error caller "sys.exec expected string arg";
              V_Error |> Value.inferred ~span)
    in
    let get_env =
      native_fn "sys.get_env" (fun ty ~caller ~state:_ arg : value ->
          let sum_variant_ty =
            match Ty.await_inferred ty.result with
            | Types.T_Variant variant_ty -> variant_ty
            | _ -> unreachable "sys.get_env returns variant type"
          in
          (* Get label `:Found` of get_env's return type *)
          let label_found, xs =
            match
              Inference.Var.inferred_opt sum_variant_ty.variants.var
              |> Option.get
            with
            | R_Cons { label; rest; _ } ->
                (* Assert label has name `Found` *)
                if Label.get_name label <> "Found" then
                  unreachable
                    "sys.get_env's return type has first variant called Found";
                (label, rest)
            | _ -> unreachable "sys.get_env returns row of 2 variants"
          in
          (* Get label `:NotFound` of get_env's return type *)
          let label_not_found =
            match Inference.Var.inferred_opt xs.var |> Option.get with
            | R_Cons { label; rest; _ } ->
                (* Assert label has name `NotFound` *)
                if Label.get_name label <> "NotFound" then
                  unreachable
                    "sys.get_env's return type has first variant called Found";
                (* Assert this is the last variant *)
                (match Inference.Var.inferred_opt rest.var |> Option.get with
                | R_Empty -> ()
                | _ -> unreachable "sys.get_env returns row of 2 variants");
                label
            | _ -> unreachable "sys.get_env returns row of 2 variants"
          in
          match arg |> Value.await_inferred with
          | V_String var -> (
              let env_val =
                try Some (Sys.getenv var) with Not_found -> None
              in
              match env_val with
              | Some s ->
                  V_Variant
                    {
                      label = label_found;
                      data =
                        Some (Place.init (V_String s |> Value.inferred ~span));
                      ty = sum_variant_ty;
                    }
                  |> Value.inferred ~span
              | None ->
                  V_Variant
                    {
                      label = label_not_found;
                      data = None;
                      ty = sum_variant_ty;
                    }
                  |> Value.inferred ~span)
          | _ ->
              Error.error caller "sys.exec expected string arg";
              V_Error |> Value.inferred ~span)
    in
    [ chdir; argc; argv_at; exec; get_env ]
  in

  let fs =
    let read_file =
      native_fn "fs.read_file" (fun _ty ~caller ~state:_ arg : value ->
          match arg |> Value.await_inferred with
          | V_String path ->
              let contents = read_from_filesystem path in
              V_String contents |> Value.inferred ~span
          | _ ->
              Error.error caller "fs.read_file expected string arg";
              V_Error |> Value.inferred ~span)
    in
    [ read_file ]
  in

  let natives : natives =
    let cmp_fn name op =
      native_fn name (fun _ty ~caller ~state:_ value ->
          match value |> Value.await_inferred with
          | V_Tuple { ty = _; tuple } ->
              let a, b = tuple |> Tuple.unwrap_unnamed2 in
              let a =
                a.place |> Common.claim ~span:caller |> Value.await_inferred
              in
              let b =
                b.place |> Common.claim ~span:caller |> Value.await_inferred
              in
              let result : bool = op a b in
              V_Bool result |> Value.inferred ~span
          | _ ->
              Error.error caller "cmp op %S expected a tuple as arg" name;
              V_Error |> Value.inferred ~span)
    in
    let bin_op name (op_int32 : int32 -> int32 -> int32)
        (op_int64 : int64 -> int64 -> int64)
        (op_float : (float -> float -> float) option) =
      native_fn name (fun _ty ~caller ~state:_ value ->
          match value |> Value.await_inferred with
          | V_Tuple { ty = _; tuple } ->
              let a, b = tuple |> Tuple.unwrap_unnamed2 in
              let result : Value.shape =
                match
                  ( a.place |> Common.claim ~span:caller |> Value.await_inferred,
                    b.place |> Common.claim ~span:caller |> Value.await_inferred
                  )
                with
                | V_Int32 a, V_Int32 b -> V_Int32 (op_int32 a b)
                | V_Int64 a, V_Int64 b -> V_Int64 (op_int64 a b)
                | V_Float64 a, V_Float64 b -> (
                    match op_float with
                    | Some op_float -> V_Float64 (op_float a b)
                    | None -> V_Error)
                | V_String a, V_String b ->
                    V_String (a ^ b) (* TODO only for + *)
                | _ -> V_Error
              in
              result |> Value.inferred ~span
          | _ ->
              Error.error caller "bin op %S expected a tuple as arg" name;
              V_Error |> Value.inferred ~span)
    in
    let list : (string * (ty -> value)) list =
      [
        native_fn "print" (fun _ty ~caller ~state:_ value ->
            (match value |> Value.await_inferred with
            | V_String s -> println "%s" s
            | _ -> Error.error caller "print expected a string");
            V_Unit |> Value.inferred ~span);
        native_fn "eprint" (fun _ty ~caller ~state:_ value ->
            (match value |> Value.await_inferred with
            | V_String s -> eprintln "%s" s
            | _ -> Error.error caller "eprint expected a string");
            V_Unit |> Value.inferred ~span);
        native_fn "input" (fun _ty ~caller ~state:_ value ->
            match value |> Value.await_inferred with
            | V_String s ->
                let line = Effect.perform (Input s) in
                V_String line |> Value.inferred ~span
            | _ ->
                Error.error caller "input expected a string";
                V_Error |> Value.inferred ~span);
        native_fn "to_string" (fun _ty ~caller ~state:_ arg ->
            with_return (fun { return } : Value.Shape.t ->
                let s =
                  match arg |> Value.await_inferred with
                  | V_Bool value -> Bool.to_string value
                  | V_Char value -> String.make 1 value
                  | V_Int32 value -> Int32.to_string value
                  | V_Int64 value -> Int64.to_string value
                  | V_Float64 value -> Float.to_string value
                  | shape ->
                      Error.error caller "to_string doesn't work for %a"
                        Value.Shape.print shape;
                      return (V_Error : Value.Shape.t)
                in
                V_String s)
            |> Value.inferred ~span);
        native_fn "parse" (fun ty ~caller ~state:_ arg ->
            let { arg = _; result = result_ty } : Types.ty_fn = ty in
            match arg |> Value.await_inferred with
            | V_String s ->
                let shape : Value.shape =
                  let parsed =
                    match result_ty |> Ty.await_inferred with
                    | T_Int32 ->
                        Int32.of_string_opt s
                        |> Option.map (fun x : Value.shape -> V_Int32 x)
                    | T_Int64 ->
                        Int64.of_string_opt s
                        |> Option.map (fun x : Value.shape -> V_Int64 x)
                    | T_Float64 ->
                        Float.of_string_opt s
                        |> Option.map (fun x : Value.shape -> V_Float64 x)
                    | _ ->
                        Error.error caller "no idea how to parse %a" Ty.print
                          result_ty;
                        Some V_Error
                  in
                  match parsed with
                  | Some value -> value
                  | None ->
                      Error.error caller "could not parse %S as %a" s Ty.print
                        result_ty;
                      V_Error
                in
                shape |> Value.inferred ~span
            | _ ->
                Error.error caller "string_to_int32 expected a string";
                V_Error |> Value.inferred ~span);
        cmp_fn "<" ( < );
        cmp_fn "<=" ( <= );
        cmp_fn "==" ( = );
        cmp_fn "!=" ( <> );
        cmp_fn ">=" ( >= );
        cmp_fn ">" ( > );
        bin_op "+" Int32.add Int64.add (Some Float.add);
        bin_op "-" Int32.sub Int64.sub (Some Float.sub);
        bin_op "*" Int32.mul Int64.mul (Some Float.mul);
        bin_op "/" Int32.div Int64.div (Some Float.div);
        bin_op "%" Int32.rem Int64.rem (Some Float.rem);
        bin_op "bit_and" Int32.logand Int64.logand None;
        bin_op "bit_or" Int32.logor Int64.logor None;
        bin_op "bit_xor" Int32.logxor Int64.logxor None;
        bin_op "bit_shift_left"
          (fun a b -> Int32.shift_left a (Int32.to_int b))
          (fun a b -> Int64.shift_left a (Int64.to_int b))
          None;
        bin_op "bit_shift_right"
          (fun a b -> Int32.shift_right a (Int32.to_int b))
          (fun a b -> Int64.shift_right a (Int64.to_int b))
          None;
        native_fn "bit_not" (fun _ty ~caller ~state:_ arg ->
            (match arg |> Value.await_inferred with
              | V_Int32 x -> V_Int32 (Int32.lognot x)
              | V_Int64 x -> V_Int64 (Int64.lognot x)
              | value ->
                  Error.error caller "bit_not doesnt work with %a"
                    Value.Shape.print value;
                  V_Error)
            |> Value.inferred ~span);
        native_fn "unary -" (fun _ty ~caller ~state:_ arg ->
            (match arg |> Value.await_inferred with
              | V_Int32 x -> V_Int32 (Int32.neg x)
              | V_Int64 x -> V_Int64 (Int64.neg x)
              | V_Float64 x -> V_Float64 (Float.neg x)
              | value ->
                  Error.error caller "unary - doesnt work with %a"
                    Value.Shape.print value;
                  V_Error)
            |> Value.inferred ~span);
        native_fn "create_context_type" (fun _ty ~caller ~state:_ arg ->
            match arg |> Value.await_inferred with
            | V_Ty ty ->
                V_ContextTy { id = Id.gen (); ty } |> Value.inferred ~span
            | _ ->
                Error.error caller "create_context_type expected a type";
                V_Error |> Value.inferred ~span);
        native_fn "panic" (fun _ty ~caller:_ ~state:_ arg ->
            match arg |> Value.expect_string with
            | Some s -> raise (Panic s)
            | None -> V_Error |> Value.inferred ~span);
      ]
      @ types @ fs @ sys @ rng @ mod_char @ mod_string @ dbg
    in
    { by_name = list |> StringMap.of_list }
  in
  natives
