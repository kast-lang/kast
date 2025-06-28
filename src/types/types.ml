open Std
open Kast_util

type _unused

(* VALUE *)
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

(* TY *)
and ty_shape =
  | T_Int32
  | T_Ty

and ty = { shape : ty_shape }

(* EXPR *)
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

and expr_scope = { expr : expr }

and expr_assign = {
  assignee : assignee_expr;
  value : expr;
}

and expr_shape =
  | E_Constant of value
  | E_Binding of binding
  | E_Then of expr_then
  | E_Scope of expr_scope
  | E_Fn of expr_fn
  | E_Tuple of expr_tuple
  | E_Apply of expr_apply
  | E_Assign of expr_assign

and expr = { shape : expr_shape }

(* ASSIGNEE EXPR *)
and assignee_expr_shape =
  | A_Placeholder
  | A_Binding of binding
  | A_Let of pattern

and assignee_expr = { shape : assignee_expr_shape }

(* PATTERN *)
and pattern_shape =
  | P_Placeholder
  | P_Binding of binding

and pattern = { shape : pattern_shape }

(* OTHER *)
and binding = { name : string }
