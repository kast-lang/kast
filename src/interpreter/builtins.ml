open Std
open Kast_util
open Kast_types

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
    ("int32", { shape = V_Ty (Ty.inferred T_Int32) });
  ]
