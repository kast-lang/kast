open Std
open Kast_util
open Kast_types

type natives = { by_name : value StringMap.t }
type t = natives

let types : (string * Ty.Shape.t) list =
  [ ("unit", T_Unit); ("int32", T_Int32); ("string", T_String); ("type", T_Ty) ]

let types =
  types
  |> List.map (fun (name, ty) : (string * value) ->
         (name, { shape = V_Ty (Ty.inferred ty) }))

let natives : natives =
  let native_fn ~(arg : ty) ~(result : ty) name impl : string * value =
    (name, { shape = V_NativeFn { ty = { arg; result }; name; impl } })
  in
  let list : (string * value) list =
    [
      native_fn ~arg:(Ty.inferred T_String) ~result:(Ty.inferred T_Unit) "print"
        (fun ~caller value ->
          (match value.shape with
          | V_String s -> println "%s" s
          | _ -> Error.error caller "print expected a string");
          { shape = V_Unit });
      native_fn ~arg:(Ty.inferred T_Int32) ~result:(Ty.inferred T_Int32) "rng"
        (fun ~caller arg ->
          match arg.shape with
          | V_Int32 max -> { shape = V_Int32 (Random.int32 max) }
          | _ ->
              Error.error caller "rng expected int32";
              { shape = V_Error });
      native_fn ~arg:(Ty.inferred T_Int32) ~result:(Ty.inferred T_String)
        "int32_to_string" (fun ~caller arg ->
          match arg.shape with
          | V_Int32 value -> { shape = V_String (Int32.to_string value) }
          | _ ->
              Error.error caller "int32_to_string expected an int32";
              { shape = V_Error });
    ]
    @ types
  in
  { by_name = list |> StringMap.of_list }
