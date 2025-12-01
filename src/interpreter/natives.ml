open Std
open Kast_util
open Kast_types
module Inference = Kast_inference

type natives = Types.natives
type t = natives
type _ Effect.t += Input : string -> string Effect.t

let plain_types : (string * Ty.Shape.t) list =
  [
    ("unit", T_Unit);
    ("int32", T_Int32);
    ("string", T_String);
    ("char", T_Char);
    ("type", T_Ty);
    ("bool", T_Bool);
  ]

let generic_types : (string * (Ty.Shape.t -> Ty.Shape.t)) list =
  [
    ( "unwind_token",
      fun result ->
        T_UnwindToken
          { result = Ty.inferred ~span:(Span.of_ocaml __POS__) result } );
  ]

let types =
  (plain_types
  |> List.map (fun (name, ty) : (string * value) ->
      ( name,
        V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) ty)
        |> Value.inferred ~span:(Span.of_ocaml __POS__) )))
  @ (generic_types
    |> List.map (fun (name, f) : (string * value) ->
        let impl =
         fun ~caller ~state:_ arg ->
          with_return (fun { return } : value ->
              let error () =
                return
                  (V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) T_Error)
                  |> Value.inferred ~span:(Span.of_ocaml __POS__))
              in
              let arg =
                arg |> Value.expect_ty
                |> Option.unwrap_or_else (fun () ->
                    Error.error caller "%S expected a type arg" name;
                    error ())
                |> fun ty -> Ty.await_inferred ty
              in

              V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) (f arg))
              |> Value.inferred ~span:(Span.of_ocaml __POS__))
        in

        ( name,
          V_NativeFn
            {
              id = Id.gen ();
              name;
              ty =
                {
                  arg = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Ty;
                  result = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Ty;
                };
              impl;
            }
          |> Value.inferred ~span:(Span.of_ocaml __POS__) )))

let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
  ( name,
    V_NativeFn { id = Id.gen (); ty = { arg; result }; name; impl }
    |> Value.inferred ~span:(Span.of_ocaml __POS__) )

let dbg =
  [
    (* TODO dbg should be polymorphic *)
    native_fn
      ~arg:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
      "dbg.print"
      (fun ~caller:_ ~state:_ arg : value ->
        println "%a" Value.print arg;
        V_Unit |> Value.inferred ~span:(Span.of_ocaml __POS__));
  ]

let mod_string =
  let get_at =
    native_fn
      ~arg:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
      "string.get_at"
      (fun ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "string.get_at: %s" msg;
              return (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
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
            |> Value.inferred ~span:(Span.of_ocaml __POS__)))
  in
  let length =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
      "string.length"
      (fun ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "string.length: %s" msg;
              return (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
            in
            let s =
              arg |> Value.expect_string
              |> Option.unwrap_or_else (error "expected string as arg")
            in
            V_Int32 (Int32.of_int (String.length s))
            |> Value.inferred ~span:(Span.of_ocaml __POS__)))
  in
  let substring =
    native_fn
      ~arg:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      ~result:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      "string.substring"
      (fun ~caller ~state arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "string.substring: %s" msg;
              return (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
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
            |> Value.inferred ~span:(Span.of_ocaml __POS__)))
  in
  let iter =
    native_fn
      ~arg:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      ~result:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      "string.iter"
      (fun ~caller ~state arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "string.iter: %s" msg;
              return (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
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
                let c : value =
                  V_Char c |> Value.inferred ~span:(Span.of_ocaml __POS__)
                in
                ignore <| Common.call caller state f.value c;
                ());
            V_Unit |> Value.inferred ~span:(Span.of_ocaml __POS__)))
  in
  [ get_at; length; substring; iter ]

let sys =
  let chdir =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
      "sys.chdir"
      (fun ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_String path ->
            Sys.chdir path;
            V_Unit |> Value.inferred ~span:(Span.of_ocaml __POS__)
        | _ ->
            Error.error caller "sys.chdir expected string arg";
            V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
  in
  [ chdir ]

let fs =
  let read_file =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      "fs.read_file"
      (fun ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_String path ->
            let contents = read_from_filesystem path in
            V_String contents |> Value.inferred ~span:(Span.of_ocaml __POS__)
        | _ ->
            Error.error caller "fs.read_file expected string arg";
            V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
  in
  [ read_file ]

let natives : natives =
  let cmp_fn name op =
    native_fn
      ~arg:
        (Ty.inferred ~span:(Span.of_ocaml __POS__)
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                         label =
                           Label.create_definition (Span.of_ocaml __POS__) "0";
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                         label =
                           Label.create_definition (Span.of_ocaml __POS__) "1";
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Bool)
      name
      (fun ~caller ~state:_ value ->
        match value |> Value.await_inferred with
        | V_Tuple { tuple } ->
            let a, b = tuple |> Tuple.unwrap_unnamed2 in
            let a = a.value |> Value.await_inferred in
            let b = b.value |> Value.await_inferred in
            let result : bool = op a b in
            V_Bool result |> Value.inferred ~span:(Span.of_ocaml __POS__)
        | _ ->
            Error.error caller "cmp op %S expected a tuple as arg" name;
            V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
  in
  let bin_op name (op : int32 -> int32 -> int32) =
    native_fn
      ~arg:
        (Ty.inferred ~span:(Span.of_ocaml __POS__)
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                         label =
                           Label.create_definition (Span.of_ocaml __POS__) "0";
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                         label =
                           Label.create_definition (Span.of_ocaml __POS__) "1";
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
      name
      (fun ~caller ~state:_ value ->
        match value |> Value.await_inferred with
        | V_Tuple { tuple } ->
            with_return (fun { return } : value ->
                let a, b = tuple |> Tuple.unwrap_unnamed2 in
                let a =
                  a.value |> Value.expect_int32
                  |> Option.unwrap_or_else (fun () ->
                      return
                        (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__)))
                in
                let b =
                  b.value |> Value.expect_int32
                  |> Option.unwrap_or_else (fun () ->
                      return
                        (V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__)))
                in
                let result : int32 = op a b in
                V_Int32 result |> Value.inferred ~span:(Span.of_ocaml __POS__))
        | _ ->
            Error.error caller "bin op %S expected a tuple as arg" name;
            V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__))
  in
  let list : (string * value) list =
    [
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
        "print"
        (fun ~caller ~state:_ value ->
          (match value |> Value.await_inferred with
          | V_String s -> println "%s" s
          | _ -> Error.error caller "print expected a string");
          V_Unit |> Value.inferred ~span:(Span.of_ocaml __POS__));
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        "input"
        (fun ~caller ~state:_ value ->
          match value |> Value.await_inferred with
          | V_String s ->
              let line = Effect.perform (Input s) in
              V_String line |> Value.inferred ~span:(Span.of_ocaml __POS__)
          | _ ->
              Error.error caller "input expected a string";
              V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__));
      native_fn
        ~arg:
          (Ty.inferred ~span:(Span.of_ocaml __POS__)
             (T_Tuple
                {
                  tuple =
                    Tuple.make []
                      [
                        ( "min",
                          ({
                             ty =
                               Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                             label =
                               Label.create_definition (Span.of_ocaml __POS__)
                                 "min";
                           }
                            : Types.ty_tuple_field) );
                        ( "max",
                          ({
                             ty =
                               Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32;
                             label =
                               Label.create_definition (Span.of_ocaml __POS__)
                                 "max";
                           }
                            : Types.ty_tuple_field) );
                      ];
                }))
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
        "rng"
        (fun ~caller ~state:_ arg ->
          try
            let { tuple } : Kast_types.Types.value_tuple =
              arg |> Value.expect_tuple |> Option.get
            in
            let min, max = tuple |> Tuple.unwrap_named2 [ "min"; "max" ] in
            let min = min.value |> Value.expect_int32 |> Option.get in
            let max = max.value |> Value.expect_int32 |> Option.get in
            V_Int32 (Random.int32_in_range ~min ~max)
            |> Value.inferred ~span:(Span.of_ocaml __POS__)
          with exc ->
            Error.error caller "rng: %s" (Printexc.to_string exc);
            V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__));
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        "int32_to_string"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Int32 value ->
              V_String (Int32.to_string value)
              |> Value.inferred ~span:(Span.of_ocaml __POS__)
          | _ ->
              Error.error caller "int32_to_string expected an int32";
              V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__));
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
        "string_to_int32"
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
              shape |> Value.inferred ~span:(Span.of_ocaml __POS__)
          | _ ->
              Error.error caller "string_to_int32 expected a string";
              V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__));
      cmp_fn "<" ( < );
      cmp_fn "<=" ( <= );
      cmp_fn "==" ( = );
      cmp_fn "!=" ( <> );
      cmp_fn ">=" ( >= );
      cmp_fn ">" ( > );
      bin_op "+" Int32.add;
      bin_op "-" Int32.sub;
      bin_op "*" Int32.mul;
      bin_op "/" Int32.div;
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Ty)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_ContextTy)
        "create_context_type"
        (fun ~caller ~state:_ arg ->
          match arg |> Value.await_inferred with
          | V_Ty ty ->
              V_ContextTy { id = Id.gen (); ty }
              |> Value.inferred ~span:(Span.of_ocaml __POS__)
          | _ ->
              Error.error caller "create_context_type expected a type";
              V_Error |> Value.inferred ~span:(Span.of_ocaml __POS__));
    ]
    @ types @ fs @ sys @ mod_string @ dbg
  in
  { by_name = list |> StringMap.of_list }
