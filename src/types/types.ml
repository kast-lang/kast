open Std
open Kast_util
module Inference = Kast_inference_base

type _unused

(* VALUE *)
and value_shape =
  | V_Unit
  | V_Bool of bool
  | V_Int32 of int32
  | V_String of string
  | V_Tuple of value_tuple
  | V_Ty of ty
  | V_Fn of value_fn
  | V_NativeFn of value_native_fn
  | V_Error

and value = { shape : value_shape }

and value_fn = {
  def : expr_fn;
  captured : interpreter_scope;
}

and value_tuple = { tuple : value tuple }

and value_native_fn = {
  name : string;
  ty : ty_fn;
  impl : value -> value;
}

and ty_tuple = { tuple : ty tuple }

and ty_fn = {
  arg : ty;
  result : ty;
}

(* TY *)
and ty_shape =
  | T_Unit
  | T_Bool
  | T_Int32
  | T_String
  | T_Tuple of ty_tuple
  | T_Ty
  | T_Fn of ty_fn
  | T_Error

and ty = { var : ty_shape Inference.var }

(* EXPR *)
and expr_fn = {
  arg : pattern;
  body : expr;
  evaled_result : ty_expr option;
}

and expr_then = {
  a : expr;
  b : expr;
}

and expr_stmt = { expr : expr }
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

and expr_native = { expr : string }
and expr_module = { def : expr }

and expr_field = {
  obj : expr;
  field : string;
}

and expr_use_dot_star = {
  used : expr;
  bindings : binding list;
}

and expr_if = {
  cond : expr;
  then_case : expr;
  else_case : expr;
}

and expr_shape =
  | E_Constant of value
  | E_Binding of binding
  | E_Then of expr_then
  | E_Stmt of expr_stmt
  | E_Scope of expr_scope
  | E_Fn of expr_fn
  | E_Tuple of expr_tuple
  | E_Apply of expr_apply
  | E_Assign of expr_assign
  | E_Ty of ty_expr
  | E_Native of expr_native
  | E_Module of expr_module
  | E_Field of expr_field
  | E_UseDotStar of expr_use_dot_star
  | E_If of expr_if
  | E_Error

and expr = {
  shape : expr_shape;
  data : ir_data;
}

(* ASSIGNEE EXPR *)
and assignee_expr_shape =
  | A_Placeholder
  | A_Unit
  | A_Binding of binding
  | A_Let of pattern
  | A_Error

and assignee_expr = {
  shape : assignee_expr_shape;
  data : ir_data;
}

(* TYPE EXPR *)
and ty_expr_fn = {
  arg : ty_expr;
  result : ty_expr;
}

and ty_expr_shape =
  | TE_Unit
  | TE_Fn of ty_expr_fn
  | TE_Expr of expr
  | TE_Error

and ty_expr = {
  shape : ty_expr_shape;
  (* TODO technically only need span for this? *)
  data : ir_data;
}

(* PATTERN *)
and pattern_shape =
  | P_Placeholder
  | P_Unit
  | P_Binding of binding
  | P_Error

and pattern = {
  shape : pattern_shape;
  data : ir_data;
}

(* SCOPE *)
and interpreter_locals = { by_symbol : value SymbolMap.t }

and interpreter_scope = {
  mutable locals : interpreter_locals;
  parent : interpreter_scope option;
}

(* OTHER *)
and binding = {
  name : symbol;
  span : span;
  ty : ty;
  (* Think: maybe this shouldnt be stored here? *)
  mutable references : span list;
}

and ir_data = {
  span : span;
  ty : ty;
  ty_ascription : ty_expr option;
  evaled_exprs : expr list;
}
