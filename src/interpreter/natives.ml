open Std
open Kast_util
open Kast_types

type natives = { by_name : value StringMap.t }
type t = natives
type _ Effect.t += Input : string -> string Effect.t

let types : (string * Ty.Shape.t) list =
  [
    ("unit", T_Unit);
    ("int32", T_Int32);
    ("string", T_String);
    ("type", T_Ty);
    ("bool", T_Bool);
  ]

let types =
  types
  |> List.map (fun (name, ty) : (string * value) ->
      (name, { shape = V_Ty (Ty.inferred ty) }))

let natives : natives =
  let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
    (name, { shape = V_NativeFn { ty = { arg; result }; name; impl } })
  in
  let cmp_fn name op =
    native_fn
      ~arg:
        (Ty.inferred
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred T_Int32;
                         span = Span.of_ocaml __POS__;
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred T_Int32;
                         span = Span.of_ocaml __POS__;
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred T_Bool) name
      (fun ~caller value ->
        match value.shape with
        | V_Tuple { tuple } ->
            let a, b = tuple |> Tuple.unwrap_unnamed2 in
            let result : bool = op a b in
            { shape = V_Bool result }
        | _ ->
            Error.error caller "cmp op %S expected a tuple as arg" name;
            { shape = V_Error })
  in
  let bin_op name (op : int32 -> int32 -> int32) =
    native_fn
      ~arg:
        (Ty.inferred
           (T_Tuple
              {
                tuple =
                  Tuple.make
                    [
                      ({
                         ty = Ty.inferred T_Int32;
                         span = Span.of_ocaml __POS__;
                       }
                        : Types.ty_tuple_field);
                      ({
                         ty = Ty.inferred T_Int32;
                         span = Span.of_ocaml __POS__;
                       }
                        : Types.ty_tuple_field);
                    ]
                    [];
              }))
      ~result:(Ty.inferred T_Int32) name
      (fun ~caller value ->
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
      native_fn ~arg:(Ty.inferred T_String) ~result:(Ty.inferred T_Unit) "print"
        (fun ~caller value ->
          (match value.shape with
          | V_String s -> println "%s" s
          | _ -> Error.error caller "print expected a string");
          { shape = V_Unit });
      native_fn ~arg:(Ty.inferred T_String) ~result:(Ty.inferred T_String)
        "input" (fun ~caller value ->
          match value.shape with
          | V_String s ->
              let line = Effect.perform (Input s) in
              { shape = V_String line }
          | _ ->
              Error.error caller "input expected a string";
              { shape = V_Error });
      native_fn
        ~arg:
          (Ty.inferred
             (T_Tuple
                {
                  tuple =
                    Tuple.make []
                      [
                        ( "min",
                          ({
                             ty = Ty.inferred T_Int32;
                             span = Span.of_ocaml __POS__;
                           }
                            : Types.ty_tuple_field) );
                        ( "max",
                          ({
                             ty = Ty.inferred T_Int32;
                             span = Span.of_ocaml __POS__;
                           }
                            : Types.ty_tuple_field) );
                      ];
                }))
        ~result:(Ty.inferred T_Int32) "rng"
        (fun ~caller arg ->
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
      native_fn ~arg:(Ty.inferred T_Int32) ~result:(Ty.inferred T_String)
        "int32_to_string" (fun ~caller arg ->
          match arg.shape with
          | V_Int32 value -> { shape = V_String (Int32.to_string value) }
          | _ ->
              Error.error caller "int32_to_string expected an int32";
              { shape = V_Error });
      native_fn ~arg:(Ty.inferred T_String) ~result:(Ty.inferred T_Int32)
        "string_to_int32" (fun ~caller arg ->
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
    ]
    @ types
  in
  { by_name = list |> StringMap.of_list }
