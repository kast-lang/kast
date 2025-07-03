open Std
open Kast_util
open Kast_types

let types : (string * Ty.Shape.t) list =
  [ ("unit", T_Unit); ("int32", T_Int32); ("string", T_String); ("type", T_Ty) ]

let types =
  types
  |> List.map (fun (name, ty) : (string * value) ->
         (name, { shape = V_Ty (Ty.inferred ty) }))

let builtins =
  let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
    (name, { shape = V_NativeFn { ty = { arg; result }; name; impl } })
  in
  [
    native_fn ~arg:(Ty.inferred T_String) ~result:(Ty.inferred T_Unit) "print"
      (fun value ->
        (match value.shape with
        | V_String s -> println "%s" s
        | _ -> fail "print expected a string");
        { shape = V_Unit });
    native_fn ~arg:(Ty.inferred T_Int32) ~result:(Ty.inferred T_Int32) "rng"
      (fun arg ->
        match arg.shape with
        | V_Int32 max -> { shape = V_Int32 (Random.int32 max) }
        | _ -> fail "print expected a string");
    native_fn ~arg:(Ty.inferred T_Int32) ~result:(Ty.inferred T_String)
      "int32_to_string" (fun arg ->
        match arg.shape with
        | V_Int32 value -> { shape = V_String (Int32.to_string value) }
        | _ -> fail "print expected a string");
  ]
  @ types
