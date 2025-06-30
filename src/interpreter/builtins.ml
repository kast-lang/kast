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
  let native_fn name impl : string * value =
    ( name,
      {
        shape =
          V_NativeFn
            {
              ty = { arg = Ty.inferred T_String; result = Ty.inferred T_Unit };
              name;
              impl;
            };
      } )
  in
  [
    native_fn "print" (fun value ->
        (match value.shape with
        | V_String s -> println "%s" s
        | _ -> fail "print expected a string");
        { shape = V_Unit });
  ]
  @ types
