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
      (name, { shape = V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) ty) })))
  @ (generic_types
    |> List.map (fun (name, f) : (string * value) ->
        let impl =
         fun ~caller ~state:_ arg ->
          with_return (fun { return } : value ->
              let error () =
                return
                  ({
                     shape =
                       V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) T_Error);
                   }
                    : value)
              in
              let arg =
                arg |> Value.expect_ty
                |> Option.unwrap_or_else (fun () ->
                    Error.error caller "%S expected a type arg" name;
                    error ())
                |> fun ty -> Inference.Var.await_inferred ty.var
              in
              {
                shape = V_Ty (Ty.inferred ~span:(Span.of_ocaml __POS__) (f arg));
              })
        in
        ( name,
          {
            shape =
              V_NativeFn
                {
                  name;
                  ty =
                    {
                      arg = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Ty;
                      result = Ty.inferred ~span:(Span.of_ocaml __POS__) T_Ty;
                    };
                  impl;
                };
          } )))

let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
  (name, { shape = V_NativeFn { ty = { arg; result }; name; impl } })

let dbg =
  [
    (* TODO dbg should be polymorphic *)
    native_fn
      ~arg:(Ty.new_not_inferred ~span:(Span.of_ocaml __POS__))
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
      "dbg.print"
      (fun ~caller:_ ~state:_ arg : value ->
        println "%a" Value.print arg;
        { shape = V_Unit });
  ]

let mod_string =
  let length =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
      "string.length"
      (fun ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "string.length: %s" msg;
              return ({ shape = V_Error } : value)
            in
            let s =
              arg |> Value.expect_string
              |> Option.unwrap_or_else (error "expected string as arg")
            in
            { shape = V_Int32 (Int32.of_int (String.length s)) }))
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
              return ({ shape = V_Error } : value)
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
            {
              shape =
                V_String (String.sub s (Int32.to_int start) (Int32.to_int len));
            }))
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
              return ({ shape = V_Error } : value)
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
                let c : value = { shape = V_Char c } in
                ignore <| Common.call caller state f.value c;
                ());
            { shape = V_Unit }))
  in
  [ length; substring; iter ]

let sys =
  let chdir =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
      "sys.chdir"
      (fun ~caller ~state:_ arg : value ->
        match arg.shape with
        | V_String path ->
            Sys.chdir path;
            { shape = V_Unit }
        | _ ->
            Error.error caller "sys.chdir expected string arg";
            { shape = V_Error })
  in
  [ chdir ]

let fs =
  let read_file =
    native_fn
      ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
      "fs.read_file"
      (fun ~caller ~state:_ arg : value ->
        match arg.shape with
        | V_String path ->
            let contents = read_from_filesystem path in
            { shape = V_String contents }
        | _ ->
            Error.error caller "fs.read_file expected string arg";
            { shape = V_Error })
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
        match value.shape with
        | V_Tuple { tuple } ->
            let a, b = tuple |> Tuple.unwrap_unnamed2 in
            let result : bool = op a.value b.value in
            { shape = V_Bool result }
        | _ ->
            Error.error caller "cmp op %S expected a tuple as arg" name;
            { shape = V_Error })
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
        match value.shape with
        | V_Tuple { tuple } ->
            with_return (fun { return } : value ->
                let a, b = tuple |> Tuple.unwrap_unnamed2 in
                let a =
                  a.value |> Value.expect_int32
                  |> Option.unwrap_or_else (fun () ->
                      return ({ shape = V_Error } : value))
                in
                let b =
                  b.value |> Value.expect_int32
                  |> Option.unwrap_or_else (fun () ->
                      return ({ shape = V_Error } : value))
                in
                let result : int32 = op a b in
                { shape = V_Int32 result })
        | _ ->
            Error.error caller "bin op %S expected a tuple as arg" name;
            { shape = V_Error })
  in
  let list : (string * value) list =
    [
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Unit)
        "print"
        (fun ~caller ~state:_ value ->
          (match value.shape with
          | V_String s -> println "%s" s
          | _ -> Error.error caller "print expected a string");
          { shape = V_Unit });
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        "input"
        (fun ~caller ~state:_ value ->
          match value.shape with
          | V_String s ->
              let line = Effect.perform (Input s) in
              { shape = V_String line }
          | _ ->
              Error.error caller "input expected a string";
              { shape = V_Error });
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
            { shape = V_Int32 (Random.int32_in_range ~min ~max) }
          with exc ->
            Error.error caller "rng: %s" (Printexc.to_string exc);
            { shape = V_Error });
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        "int32_to_string"
        (fun ~caller ~state:_ arg ->
          match arg.shape with
          | V_Int32 value -> { shape = V_String (Int32.to_string value) }
          | _ ->
              Error.error caller "int32_to_string expected an int32";
              { shape = V_Error });
      native_fn
        ~arg:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_String)
        ~result:(Ty.inferred ~span:(Span.of_ocaml __POS__) T_Int32)
        "string_to_int32"
        (fun ~caller ~state:_ arg ->
          match arg.shape with
          | V_String s ->
              let shape : Value.shape =
                match Int32.of_string_opt s with
                | Some value -> V_Int32 value
                | None ->
                    Error.error caller "could not parse int32 %S" s;
                    V_Error
              in
              { shape }
          | _ ->
              Error.error caller "string_to_int32 expected a string";
              { shape = V_Error });
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
          match arg.shape with
          | V_Ty ty -> { shape = V_ContextTy { id = Id.gen (); ty } }
          | _ ->
              Error.error caller "create_context_type expected a type";
              { shape = V_Error });
    ]
    @ types @ fs @ sys @ mod_string @ dbg
  in
  { by_name = list |> StringMap.of_list }
