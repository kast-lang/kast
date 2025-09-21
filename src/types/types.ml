open Std
open Kast_util
module Ast = Kast_ast
module Syntax = Kast_syntax
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
  | V_Ast of Ast.t
  | V_UnwindToken of value_unwind_token
  | V_Target of value_target
  | V_Error

and value = { shape : value_shape }
and value_target = { name : string }

and value_unwind_token = {
  id : Id.t;
  result_ty : ty;
}

and value_fn = {
  def : expr_fn;
  captured : interpreter_scope;
}

and value_tuple = { tuple : value tuple }

and value_native_fn = {
  name : string;
  ty : ty_fn;
  impl : caller:span -> value -> value;
}

(* TY *)
and ty_tuple = { tuple : ty tuple }

and ty_fn = {
  arg : ty;
  result : ty;
}

and ty_unwind_token = { result : ty }

and ty_shape =
  | T_Unit
  | T_Bool
  | T_Int32
  | T_String
  | T_Tuple of ty_tuple
  | T_Ty
  | T_Fn of ty_fn
  | T_Ast
  | T_UnwindToken of ty_unwind_token
  | T_Target
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

and expr_quote_ast_child =
  | Group of expr_quote_ast_group
  | Ast of expr

and expr_quote_ast_group = {
  rule : Syntax.Rule.group option;
  children : expr_quote_ast_child tuple;
}

and expr_quote_ast = {
  rule : Syntax.rule;
  root : expr_quote_ast_group;
}

and expr_loop = { body : expr }

and expr_unwindable = {
  token : pattern;
  body : expr;
}

and expr_unwind = {
  token : expr;
  value : expr;
}

and expr_target_dependent = { branches : expr_target_dependent_branch list }

and expr_target_dependent_branch = {
  cond : expr;
  body : expr;
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
  | E_QuoteAst of expr_quote_ast
  | E_Loop of expr_loop
  | E_Unwindable of expr_unwindable
  | E_Unwind of expr_unwind
  | E_TargetDependent of expr_target_dependent
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

and ty_expr_tuple = { tuple : ty_expr tuple }

and ty_expr_shape =
  | TE_Unit
  | TE_Fn of ty_expr_fn
  | TE_Expr of expr
  | TE_Tuple of ty_expr_tuple
  | TE_Error

and ty_expr = {
  shape : ty_expr_shape;
  (* TODO technically only need span for this? *)
  data : ir_data;
}

(* PATTERN *)
and pattern_tuple = { tuple : pattern tuple }

and pattern_shape =
  | P_Placeholder
  | P_Unit
  | P_Binding of binding
  | P_Tuple of pattern_tuple
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
  compiler_scope : compiler_scope;
  ty_ascription : ty_expr option;
  evaled_exprs : expr list;
}

and compiler_scope = {
  parent : compiler_scope option;
  bindings : binding StringMap.t;
}

let target_symbol : symbol = Symbol.create "target"
