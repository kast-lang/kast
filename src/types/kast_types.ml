open Std
open Kast_util

type _unused

and value_shape =
  | V_Unit
  | V_Int32 of int32
  | V_String of string
  | V_Tuple of value_tuple
  | V_Ty of ty
  | V_Fn of value_fn
  | V_NativeFn of value_native_fn

and value = { shape : value_shape }
and value_fn = expr_fn
and value_tuple = { tuple : value tuple }

and value_native_fn = {
  name : string;
  impl : value -> value;
}

and ty_shape =
  | T_Int32
  | T_Ty

and ty = { shape : ty_shape }

and expr_fn = {
  arg : pattern;
  body : expr;
}

and expr_then = {
  a : expr;
  b : expr;
}

and expr_tuple = { tuple : expr tuple }

and expr_apply = {
  f : expr;
  arg : expr;
}

and expr_shape =
  | E_Constant of value
  | E_Binding of binding
  | E_Then of expr_then
  | E_Fn of expr_fn
  | E_Tuple of expr_tuple
  | E_Apply of expr_apply

and expr = { shape : expr_shape }

and pattern_shape =
  | P_Placeholder
  | P_Binding of binding

and pattern = { shape : pattern_shape }
and binding = { name : string }

module Value = struct
  module Shape = struct
    let print : formatter -> value_shape -> unit =
     fun fmt -> function
      | V_Unit -> fprintf fmt "()"
      | V_Ty ty -> fail "todo"
      | V_Int32 value -> fprintf fmt "@{<italic>%s@}" (Int32.to_string value)
      | V_String value -> fprintf fmt "@{<green>%S@}" value
      | V_Tuple _ -> fail "todo"
      | V_Fn f -> fail "todo"
      | V_NativeFn f -> fprintf fmt "@{<italic><native %s>@}" f.name
  end

  let print : formatter -> value -> unit =
   fun fmt value -> Shape.print fmt value.shape
end
