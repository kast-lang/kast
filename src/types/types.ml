open Std
open Kast_util
module Ast = Kast_ast
module Syntax = Kast_syntax
module Inference = Kast_inference_base
module Label = Label

type _unused

(* VALUE *)
and value_shape =
  | V_Unit
  | V_Bool of bool
  | V_Int32 of int32
  | V_Char of char
  | V_String of string
  | V_Tuple of value_tuple
  | V_Ty of ty
  | V_Fn of value_fn
  | V_Generic of value_generic
  | V_NativeFn of value_native_fn
  | V_Ast of Ast.t
  | V_UnwindToken of value_unwind_token
  | V_Target of value_target
  | V_ContextTy of value_context_ty
  | V_Binding of binding
  | V_CompilerScope of compiler_scope
  | V_Error

and value = { shape : value_shape }
and value_target = { name : string }

and value_context_ty = {
  id : Id.t;
  ty : ty;
}

and value_unwind_token = {
  id : Id.t;
  result_ty : ty;
}

and value_fn = {
  ty : ty_fn;
  fn : value_untyped_fn;
}

and value_untyped_fn = {
  def : maybe_compiled_fn;
  captured : interpreter_scope;
}

and value_generic = { fn : value_untyped_fn }

and value_tuple_field = {
  value : value;
  span : span;
  ty_field : ty_tuple_field;
}

and value_tuple = { tuple : value_tuple_field tuple }

and value_native_fn = {
  name : string;
  ty : ty_fn;
  impl : caller:span -> state:interpreter_state -> value -> value;
}

(* TY *)
and ty_tuple_field = {
  ty : ty;
  label : Label.t;
}

and ty_tuple = { tuple : ty_tuple_field tuple }

and ty_fn = {
  arg : ty;
  result : ty;
}

and ty_generic = { fn : value_untyped_fn }
and ty_unwind_token = { result : ty }

and ty_shape =
  | T_Unit
  | T_Bool
  | T_Int32
  | T_String
  | T_Char
  | T_Tuple of ty_tuple
  | T_Ty
  | T_Fn of ty_fn
  | T_Generic of ty_generic
  | T_Ast
  | T_UnwindToken of ty_unwind_token
  | T_Target
  | T_ContextTy
  | T_CompilerScope
  | T_Binding of binding
  | T_Error

and ty = { var : ty_shape Inference.var }

(* EXPR *)
and compiled_fn = {
  arg : pattern;
  body : expr;
  evaled_result : ty_expr option;
}

and maybe_compiled_fn = { mutable compiled : compiled_fn option }

and expr_fn = {
  ty : ty_fn;
  def : maybe_compiled_fn;
}

and expr_generic = { def : maybe_compiled_fn }

and expr_then = {
  a : expr;
  b : expr;
}

and 'a tuple_field_of = field_span:span * field_label:Label.t * 'a
and expr_stmt = { expr : expr }
and expr_tuple = { tuple : expr tuple_field_of tuple }

and expr_apply = {
  f : expr;
  arg : expr;
}

and expr_instantiate_generic = {
  generic : expr;
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
  field_span : span;
  label : Label.t;
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

and expr_inject_context = {
  context_ty : value_context_ty;
  value : expr;
}

and expr_current_context = { context_ty : value_context_ty }

and expr_shape =
  | E_Constant of value
  | E_Binding of binding
  | E_Then of expr_then
  | E_Stmt of expr_stmt
  | E_Scope of expr_scope
  | E_Fn of expr_fn
  | E_Generic of expr_generic
  | E_Tuple of expr_tuple
  | E_Apply of expr_apply
  | E_InstantiateGeneric of expr_instantiate_generic
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
  | E_InjectContext of expr_inject_context
  | E_CurrentContext of expr_current_context
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

and ty_expr_tuple = { tuple : ty_expr tuple_field_of tuple }

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
and pattern_tuple = { tuple : pattern tuple_field_of tuple }

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
and interpreter_local = {
  mutable value : value;
  ty_field : ty_tuple_field;
}

and interpreter_locals = { by_symbol : interpreter_local SymbolMap.t }

and interpreter_scope = {
  mutable locals : interpreter_locals;
  parent : interpreter_scope option;
}

(* interpreter *)
and natives = { by_name : value StringMap.t }

and interpreter_state = {
  natives : natives;
  scope : interpreter_scope;
  mutable contexts : value Id.Map.t;
}

(* OTHER *)
and binding = {
  name : symbol;
  span : span;
  ty : ty;
  label : Label.t;
}

and ir_data = {
  span : span;
  ty : ty;
  compiler_scope : compiler_scope;
  mutable ty_ascription : ty_expr option;
  mutable evaled_exprs : expr list;
  included_file : Uri.t option;
}

and compiler_scope = {
  id : id;
  parent : compiler_scope option;
  recursive : bool;
  mutable bindings : binding StringMap.t;
  mutable closed : bool;
  mutable on_update : (unit -> unit) list;
}

let target_symbol : symbol = Symbol.create "target"
