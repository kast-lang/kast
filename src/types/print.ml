open Std
open Kast_util
open Types

let rec print_value_shape : formatter -> value_shape -> unit =
 fun fmt -> function
  | V_Unit -> fprintf fmt "()"
  | V_Ty ty -> print_ty fmt ty
  | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
  | V_String value -> fprintf fmt "@{<green>%S@}" value
  | V_Tuple { tuple } -> fprintf fmt "%a" (Tuple.print print_value) tuple
  | V_Fn f -> fprintf fmt "@{<italic><fn>@}"
  | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name

and print_value : formatter -> value -> unit =
 fun fmt value -> print_value_shape fmt value.shape

and print_ty_shape : formatter -> ty_shape -> unit =
 fun fmt -> function
  | T_Ty -> fprintf fmt "type"
  | T_Int32 -> fprintf fmt "int32"

and print_ty : formatter -> ty -> unit =
 fun fmt ty -> print_ty_shape fmt ty.shape
