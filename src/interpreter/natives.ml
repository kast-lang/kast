open Std
open Kast_util
open Kast_types
module Inference = Kast_inference

type natives = Types.natives
type t = natives
type _ Effect.t += Input : string -> string Effect.t

let argv : string array ref = ref [||]
let span = Span.of_ocaml __POS__

let plain_types : (string * Ty.Shape.t) list =
  [
    ("unit", T_Unit);
    ("int32", T_Int32);
    ("int64", T_Int64);
    ("string", T_String);
    ("char", T_Char);
    ("type", T_Ty);
    ("bool", T_Bool);
  ]

let generic_types : (string * (Ty.Shape.t -> Ty.Shape.t)) list =
  [
    ( "unwind_token",
      fun result -> T_UnwindToken { result = Ty.inferred ~span result } );
  ]

let types =
  (plain_types
  |> List.map (fun (name, ty) : (string * value) ->
      (name, V_Ty (Ty.inferred ~span ty) |> Value.inferred ~span)))
  @ (generic_types
    |> List.map (fun (name, f) : (string * value) ->
        let impl =
         fun ~caller ~state:_ arg ->
          with_return (fun { return } : value ->
              let error () =
                return (V_Ty (Ty.inferred ~span T_Error) |> Value.inferred ~span)
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
          V_NativeFn
            {
              id = Id.gen ();
              name;
              ty =
                {
                  arg = Ty.inferred ~span T_Ty;
                  result = Ty.inferred ~span T_Ty;
                };
              impl;
            }
          |> Value.inferred ~span )))

let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
  ( name,
    V_NativeFn { id = Id.gen (); ty = { arg; result }; name; impl }
    |> Value.inferred ~span )

let dbg =
  [
    (* TODO dbg should be polymorphic *)
    native_fn ~arg:(Ty.new_not_inferred ~span)
      ~result:(Ty.inferred ~span T_Unit) "dbg.print"
      (fun ~caller:_ ~state:_ arg : value ->
        println "%a" Value.print arg;
        V_Unit |> Value.inferred ~span);
  ]

let mod_char =
  let code =
    native_fn ~arg:(Ty.inferred ~span T_Char)
      ~result:(Ty.inferred ~span T_Int32) "char.code"
      (fun ~caller ~state:_ arg : value ->
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
    native_fn ~arg:(Ty.inferred ~span T_Int32)
      ~result:(Ty.inferred ~span T_Char) "char.from_code"
      (fun ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "char.from_code: %s" msg;
              return (V_Error |> Value.inferred ~span)
            in
            let code =
              arg |> Value.expect_int32
              |> Option.unwrap_or_else
                   (error
                      (make_string "arg must be uint32, got %a" Value.print arg))
            in
            V_Char (Char.chr (Int32.to_int code)) |> Value.inferred ~span))
  in
  [ code; from_code ]

let mod_string =
  let at =
    native_fn ~arg:(Ty.new_not_inferred ~span)
      ~result:(Ty.inferred ~span T_Int32) "string.at"
      (fun ~caller ~state:_ arg : value ->
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
              s.value |> Value.expect_string
              |> Option.unwrap_or_else (error "expected string as first arg")
            in
            let idx =
              idx.value |> Value.expect_int32
              |> Option.unwrap_or_else (error "expected idx be int32")
            in
            V_Char
              (String.get s (Int32.to_int idx)
              |> Option.unwrap_or_else (error "oob"))
            |> Value.inferred ~span))
  in
  let length =
    native_fn ~arg:(Ty.inferred ~span T_String)
      ~result:(Ty.inferred ~span T_Int32) "string.length"
      (fun ~caller ~state:_ arg : value ->
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
    native_fn ~arg:(Ty.new_not_inferred ~span)
      ~result:(Ty.new_not_inferred ~span) "string.substring"
      (fun ~caller ~state arg : value ->
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
              s.value |> Value.expect_string
              |> Option.unwrap_or_else (error "expected string as first arg")
            in
            let start =
              start.value |> Value.expect_int32
              |> Option.unwrap_or_else (error "expected start be int32")
            in
            let len =
              len.value |> Value.expect_int32
              |> Option.unwrap_or_else (error "expected len be int32")
            in
            V_String (String.sub s (Int32.to_int start) (Int32.to_int len))
            |> Value.inferred ~span))
  in
  let iter =
    native_fn ~arg:(Ty.new_not_inferred ~span)
      ~result:(Ty.new_not_inferred ~span) "string.iter"
      (fun ~caller ~state arg : value ->
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
              s.value |> Value.expect_string
              |> Option.unwrap_or_else (error "expected string as first arg")
            in
            let _ =
              f.value |> Value.expect_fn
              |> Option.unwrap_or_else (error "expected fn as second arg")
            in
            s
            |> String.iter (fun c ->
                let c : value = V_Char c |> Value.inferred ~span in
                ignore <| Common.call caller state f.value c;
                ());
            V_Unit |> Value.inferred ~span))
  in
  [ at; length; substring; iter ]

let sys =
  let chdir =
    native_fn ~arg:(Ty.inferred ~span T_String)
      ~result:(Ty.inferred ~span T_Unit) "sys.chdir"
      (fun ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_String path ->
            Sys.chdir path;
            V_Unit |> Value.inferred ~span
        | _ ->
            Error.error caller "sys.chdir expected string arg";
            V_Error |> Value.inferred ~span)
  in
  let argc =
    native_fn ~arg:(Ty.inferred ~span T_Unit)
      ~result:(Ty.inferred ~span T_Int32) "sys.argc"
      (fun ~caller:_ ~state:_ _arg : value ->
        V_Int32 (!argv |> Array.length |> Int32.of_int) |> Value.inferred ~span)
  in
  let argv_at =
    native_fn ~arg:(Ty.inferred ~span T_Int32)
      ~result:(Ty.inferred ~span T_String) "sys.argv_at"
      (fun ~caller ~state:_ arg : value ->
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
  [ chdir; argc; argv_at ]

exception Panic of string

let fs =
  let read_file =
    native_fn ~arg:(Ty.inferred ~span T_String)
      ~result:(Ty.inferred ~span T_String) "fs.read_file"
      (fun ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_String path ->
            let contents = read_from_filesystem path in
            V_String contents |> Value.inferred ~span
        | _ ->
            Error.error caller "fs.read_file expected string arg";
            V_Error |> Value.inferred ~span)
  in
  [ read_file ]

let natives : natives =
  let cmp_fn name op =
    native_fn
      ~arg:
        (Ty.inferred ~span
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred ~span T_Int32;
                         label = Label.create_definition span "0";
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred ~span T_Int32;
                         label = Label.create_definition span "1";
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred ~span T_Bool) name
      (fun ~caller ~state:_ value ->
        match value |> Value.await_inferred with
        | V_Tuple { tuple } ->
            let a, b = tuple |> Tuple.unwrap_unnamed2 in
            let a = a.value |> Value.await_inferred in
            let b = b.value |> Value.await_inferred in
            let result : bool = op a b in
            V_Bool result |> Value.inferred ~span
        | _ ->
            Error.error caller "cmp op %S expected a tuple as arg" name;
            V_Error |> Value.inferred ~span)
  in
  let bin_op name (op_int32 : int32 -> int32 -> int32)
      (op_int64 : int64 -> int64 -> int64) =
    native_fn
      ~arg:
        (Ty.inferred ~span
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred ~span T_Int32;
                         label = Label.create_definition span "0";
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred ~span T_Int32;
                         label = Label.create_definition span "1";
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred ~span T_Int32)
      name
      (fun ~caller ~state:_ value ->
        match value |> Value.await_inferred with
        | V_Tuple { tuple } ->
            with_return (fun { return } : value ->
                let a, b = tuple |> Tuple.unwrap_unnamed2 in
                let result : Value.shape =
                  match
                    ( a.value |> Value.await_inferred,
                      b.value |> Value.await_inferred )
                  with
                  | V_Int32 a, V_Int32 b -> V_Int32 (op_int32 a b)
                  | V_Int64 a, V_Int64 b -> V_Int64 (op_int64 a b)
                  | V_String a, V_String b ->
                      V_String (a ^ b) (* TODO only for + *)
                  | _ -> return (V_Error |> Value.inferred ~span)
                in
                result |> Value.inferred ~span)
        | _ ->
            Error.error caller "bin op %S expected a tuple as arg" name;
            V_Error |> Value.inferred ~span)
  in
  let list : (string * value) list =
    [
      native_fn ~arg:(Ty.inferred ~span T_String)
        ~result:(Ty.inferred ~span T_Unit) "print"
        (fun ~caller ~state:_ value ->
          (match value |> Value.await_inferred with
          | V_String s -> println "%s" s
          | _ -> Error.error caller "print expected a string");
          V_Unit |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_String)
        ~result:(Ty.inferred ~span T_String) "input"
        (fun ~caller ~state:_ value ->
          match value |> Value.await_inferred with
          | V_String s ->
              let line = Effect.perform (Input s) in
              V_String line |> Value.inferred ~span
          | _ ->
              Error.error caller "input expected a string";
              V_Error |> Value.inferred ~span);
      native_fn
        ~arg:
          (Ty.inferred ~span
             (T_Tuple
                {
                  tuple =
                    Tuple.make []
                      [
                        ( "min",
                          ({
                             ty = Ty.inferred ~span T_Int32;
                             label = Label.create_definition span "min";
                           }
                            : Types.ty_tuple_field) );
                        ( "max",
                          ({
                             ty = Ty.inferred ~span T_Int32;
                             label = Label.create_definition span "max";
                           }
                            : Types.ty_tuple_field) );
                      ];
                }))
        ~result:(Ty.inferred ~span T_Int32)
        "rng"
        (fun ~caller ~state:_ arg ->
          try
            let { tuple } : Kast_types.Types.value_tuple =
              arg |> Value.expect_tuple |> Option.get
            in
            let min, max = tuple |> Tuple.unwrap_named2 [ "min"; "max" ] in
            let min = min.value |> Value.expect_int32 |> Option.get in
            let max = max.value |> Value.expect_int32 |> Option.get in
            V_Int32 (Random.int32_in_range ~min ~max) |> Value.inferred ~span
          with exc ->
            Error.error caller "rng: %s" (Printexc.to_string exc);
            V_Error |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_Int32)
        ~result:(Ty.inferred ~span T_String) "int32_to_string"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Int32 value ->
              V_String (Int32.to_string value) |> Value.inferred ~span
          | _ ->
              Error.error caller "int32_to_string expected an int32";
              V_Error |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_String)
        ~result:(Ty.inferred ~span T_Int32) "string_to_int32"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_String s ->
              let shape : Value.shape =
                match Int32.of_string_opt s with
                | Some value -> V_Int32 value
                | None ->
                    Error.error caller "could not parse int32 %S" s;
                    V_Error
              in
              shape |> Value.inferred ~span
          | _ ->
              Error.error caller "string_to_int32 expected a string";
              V_Error |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_Int64)
        ~result:(Ty.inferred ~span T_String) "int64_to_string"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Int64 value ->
              V_String (Int64.to_string value) |> Value.inferred ~span
          | _ ->
              Error.error caller "int64_to_string expected an int64";
              V_Error |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_String)
        ~result:(Ty.inferred ~span T_Int64) "string_to_int64"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_String s ->
              let shape : Value.shape =
                match Int64.of_string_opt s with
                | Some value -> V_Int64 value
                | None ->
                    Error.error caller "could not parse int64 %S" s;
                    V_Error
              in
              shape |> Value.inferred ~span
          | _ ->
              Error.error caller "string_to_int64 expected a string";
              V_Error |> Value.inferred ~span);
      cmp_fn "<" ( < );
      cmp_fn "<=" ( <= );
      cmp_fn "==" ( = );
      cmp_fn "!=" ( <> );
      cmp_fn ">=" ( >= );
      cmp_fn ">" ( > );
      bin_op "+" Int32.add Int64.add;
      bin_op "-" Int32.sub Int64.sub;
      bin_op "*" Int32.mul Int64.mul;
      bin_op "/" Int32.div Int64.div;
      native_fn ~arg:(Ty.inferred ~span T_Ty)
        ~result:(Ty.inferred ~span T_ContextTy) "create_context_type"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Ty ty ->
              V_ContextTy { id = Id.gen (); ty } |> Value.inferred ~span
          | _ ->
              Error.error caller "create_context_type expected a type";
              V_Error |> Value.inferred ~span);
      native_fn ~arg:(Ty.inferred ~span T_String)
        ~result:(Ty.new_not_inferred ~span) "panic"
        (fun ~caller:_ ~state:_ arg ->
          match arg |> Value.expect_string with
          | Some s -> raise (Panic s)
          | None -> V_Error |> Value.inferred ~span);
    ]
    @ types @ fs @ sys @ mod_char @ mod_string @ dbg
  in
  { by_name = list |> StringMap.of_list }
