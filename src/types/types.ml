open Std
open Kast_util
module Syntax = Kast_syntax
module Inference = Kast_inference_base
module Label = Label

module rec TypesImpl : sig
  (* VAR *)
  type 'a var = ('a, var_scope) Inference.var
  and var_scope = interpreter_scope option

  (* PLACE *)
  and place =
    { id : Id.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mutable state : place_state
    ; ty : ty [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mut : place_mut [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and place_mut =
    | Immutable
    | Mutable
    | Inherit

  and place_state =
    | Uninitialized
    | Occupied of value
    | MovedOut

  (* blocked_value *)
  and blocked_value =
    { shape : blocked_value_shape
    ; ty : ty
    }

  and blocked_value_shape =
    | BV_Binding of binding
    | BV_Instantiate of blocked_value_instantiate
    | BV_ClaimRef of blocked_value
    | BV_FieldRef of blocked_value_field_ref

  and blocked_value_field_ref =
    { obj_ref : blocked_value
    ; member : Tuple.Member.t
    }

  and blocked_value_instantiate =
    { generic : blocked_value
    ; arg : value
    }

  (* VALUE *)
  and value_shape =
    | V_Unit
    | V_Bool of bool
    | V_Int32 of int32
    | V_Int64 of int64
    | V_Float64 of float
    | V_Char of char
    | V_Ref of value_ref
    | V_String of string
    | V_Tuple of value_tuple
    | V_Variant of value_variant
    | V_Ty of ty
    | V_Fn of value_fn
    | V_Generic of value_generic
    | V_NativeFn of value_native_fn
    | V_Ast of Ast.t
    | V_UnwindToken of value_unwind_token
    | V_Target of value_target
    | V_ContextTy of value_context_ty
    | V_CompilerScope of compiler_scope
    | V_Opaque of value_opaque
    | V_Blocked of blocked_value
    | V_Error

  and value_ref =
    { mut : bool
    ; place : place
    }

  and value =
    { var : value_shape var
    ; mutable ty : ty option
    }

  and value_opaque =
    { ty : ty_opaque
    ; value : Obj.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and value_target = { name : string }

  and value_context_ty =
    { id : Id.t
    ; ty : ty
    }

  and value_unwind_token =
    { id : Id.t
    ; result_ty : ty
    }

  and value_fn =
    { ty : ty_fn
    ; fn : value_untyped_fn
    }

  and value_untyped_fn =
    { id : Id.t
    ; def : maybe_compiled_fn
    ; captured : interpreter_scope
    ; monomorphization_state : monomorphization_state
    }

  and monomorphization_state =
    { native : (id, value) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; ty : (id, ty) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; value : (id, value) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and value_generic =
    { name : name_shape
    ; fn : value_untyped_fn
    ; ty : ty_generic
    }

  and value_tuple_field =
    { place : place
    ; span : Span.t
    ; ty_field : ty_tuple_field
    }

  and value_tuple =
    { ty : ty_tuple
    ; tuple : value_tuple_field Tuple.t
    }

  and value_variant =
    { label : Label.t
    ; data : place option
    ; ty : ty_variant
    }

  and value_native_fn =
    { id : Id.t
    ; name : string
    ; ty : ty_fn
    ; impl : caller:span -> state:interpreter_state -> value -> value
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  (* TY *)
  and ty_tuple_field =
    { ty : ty
    ; symbol : Symbol.t option
    ; label : Label.t option
    }

  and ty_tuple =
    { name : optional_name
    ; tuple : ty_tuple_field Tuple.t
    }

  and ty_fn =
    { arg : ty
    ; result : ty
    }

  and ty_generic =
    { arg : pattern
    ; result : ty
    }

  and ty_unwind_token = { result : ty }
  and ty_variant_data = { data : ty option }

  and ty_variant =
    { name : optional_name
    ; variants : (ty_variant_data, var_scope) Row.t
    }

  and ty_opaque = { name : name }
  and is_mutable = { var : (bool, var_scope) Inference.var }

  and ty_ref =
    { mut : is_mutable
    ; referenced : ty
    }

  and ty_shape =
    | T_Unit
    | T_Bool
    | T_Int32
    | T_Int64
    | T_Float64
    | T_String
    | T_Char
    | T_Ref of ty_ref
    | T_Variant of ty_variant
    | T_Tuple of ty_tuple
    | T_Ty
    | T_Fn of ty_fn
    | T_Generic of ty_generic
    | T_Ast
    | T_UnwindToken of ty_unwind_token
    | T_Target
    | T_ContextTy
    | T_CompilerScope
    | T_Opaque of ty_opaque
    | T_Blocked of blocked_value
    | T_Error

  and ty = { var : ty_shape var }

  (* EXPR *)
  and compiled_fn =
    { arg : pattern
    ; body : expr
    }

  and maybe_compiled_fn =
    { span : Span.t
    ; mutable compiled : compiled_fn option
    ; mutable on_compiled : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and expr_fn =
    { ty : ty_fn
    ; def : maybe_compiled_fn
    }

  and expr_generic =
    { def : maybe_compiled_fn
    ; ty : ty_generic
    }

  and expr_then = { list : expr list }

  and 'a tuple_field_of =
    { label_span : Span.t
    ; label : Label.t option
    ; field : 'a
    }

  and expr_stmt = { expr : expr }

  and 'a tuple_part_of =
    | Field of 'a tuple_field_of
    | Unpack of 'a

  and 'a tuple_of = { parts : 'a tuple_part_of list }
  and expr_tuple = expr tuple_of

  and expr_variant =
    { label : Label.t
    ; label_span : Span.t
    ; value : expr option
    }

  and expr_apply =
    { f : expr
    ; arg : expr
    }

  and expr_instantiate_generic =
    { generic : expr
    ; arg : expr
    }

  and expr_scope = { expr : expr }

  and expr_assign =
    { assignee : assignee_expr
    ; value : place_expr
    }

  and expr_native =
    { id : Id.t
    ; expr : string
    }

  and expr_module =
    { def : expr
    ; bindings : binding list
    }

  and expr_use_dot_star =
    { used : expr
    ; bindings : binding list
    }

  and expr_if =
    { cond : expr
    ; then_case : expr
    ; else_case : expr
    }

  and expr_match_branch =
    { pattern : pattern
    ; body : expr
    }

  and expr_match =
    { value : place_expr
    ; branches : expr_match_branch list
    }

  and expr_quote_ast_child =
    | Group of expr_quote_ast_group
    | Ast of expr

  and expr_quote_ast_group =
    { rule : Syntax.Rule.group option
    ; children : expr_quote_ast_child Tuple.t
    ; span : Span.t
    }

  and expr_quote_ast =
    { rule : Syntax.Rule.t
    ; root : expr_quote_ast_group
    }

  and expr_loop = { body : expr }

  and expr_unwindable =
    { token : pattern
    ; body : expr
    }

  and expr_unwind =
    { token : expr
    ; value : expr
    }

  and expr_target_dependent =
    { branches : expr_target_dependent_branch list
    ; captured : interpreter_scope
    ; mutable interpreter_branch : expr_target_dependent_branch option
    }

  and expr_target_dependent_branch =
    { cond : expr
    ; body : expr
    }

  and expr_inject_context =
    { context_ty : value_context_ty
    ; value : expr
    }

  and expr_current_context = { context_ty : value_context_ty }

  and expr_and =
    { lhs : expr
    ; rhs : expr
    }

  and expr_or =
    { lhs : expr
    ; rhs : expr
    }

  and expr_impl_cast =
    { value : expr
    ; target : value
    ; impl : expr
    }

  and expr_cast =
    { value : expr
    ; target : value
    }

  and expr_ref =
    { mut : bool
    ; place : place_expr
    }

  and expr_constant =
    { id : Id.t
    ; value : value
    }

  and expr_shape =
    | E_Constant of expr_constant
    | E_Ref of expr_ref
    | E_Claim of place_expr
    | E_Then of expr_then
    | E_Stmt of expr_stmt
    | E_Scope of expr_scope
    | E_Fn of expr_fn
    | E_Generic of expr_generic
    | E_Tuple of expr_tuple
    | E_Variant of expr_variant
    | E_Apply of expr_apply
    | E_InstantiateGeneric of expr_instantiate_generic
    | E_Assign of expr_assign
    | E_Ty of ty_expr
    | E_Newtype of ty_expr
    | E_Native of expr_native
    | E_Module of expr_module
    | E_UseDotStar of expr_use_dot_star
    | E_If of expr_if
    | E_And of expr_and
    | E_Or of expr_or
    | E_Match of expr_match
    | E_QuoteAst of expr_quote_ast
    | E_Loop of expr_loop
    | E_Unwindable of expr_unwindable
    | E_Unwind of expr_unwind
    | E_InjectContext of expr_inject_context
    | E_CurrentContext of expr_current_context
    | E_ImplCast of expr_impl_cast
    | E_Cast of expr_cast
    | E_TargetDependent of expr_target_dependent
    | E_Error

  and expr =
    { shape : expr_shape
    ; data : ir_data
    }

  (* PLACE EXPR *)
  and field_expr =
    | Index of int
    | Name of Label.t
    | Expr of expr

  and place_expr_field =
    { obj : place_expr
    ; field : field_expr
    ; field_span : Span.t
    }

  and place_expr_shape =
    | PE_Binding of binding
    | PE_Field of place_expr_field
    | PE_Deref of expr
    | PE_Temp of expr
    | PE_Error

  and place_expr =
    { shape : place_expr_shape
    ; mut : is_mutable
    ; data : ir_data
    }

  (* ASSIGNEE EXPR *)
  and assignee_expr_shape =
    | A_Placeholder
    | A_Unit
    | A_Tuple of assignee_expr_tuple
    | A_Place of place_expr
    | A_Let of pattern
    | A_Error

  and assignee_expr_tuple = assignee_expr tuple_of

  and assignee_expr =
    { shape : assignee_expr_shape
    ; data : ir_data
    }

  (* TYPE EXPR *)
  and ty_expr_fn =
    { arg : ty_expr
    ; result : ty_expr
    }

  and ty_expr_tuple = ty_expr tuple_of
  and ty_expr_union = { elements : ty_expr list }

  and ty_expr_variant_variant =
    { label_span : Span.t
    ; label : Label.t
    ; value : ty_expr option
    }

  and ty_expr_variant = { variants : ty_expr_variant_variant list }

  and ty_expr_ref =
    { mut : is_mutable
    ; referenced : ty_expr
    }

  and ty_expr_shape =
    | TE_Unit
    | TE_Ref of ty_expr_ref
    | TE_Fn of ty_expr_fn
    | TE_Expr of expr
    | TE_Tuple of ty_expr_tuple
    | TE_Variant of ty_expr_variant
    | TE_Union of ty_expr_union
    | TE_Error

  and ty_expr =
    { mutable compiled_shape : ty_expr_shape option
    ; mutable on_compiled : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; (* TODO technically only need span for this? *)
      data : ir_data
    }

  (* PATTERN *)
  and pattern_tuple = pattern tuple_of

  and pattern_variant =
    { label : Label.t
    ; label_span : Span.t
    ; value : pattern option
    }

  and bind_mode =
    | Claim
    | ByRef of { mut : bool }

  and pattern_binding =
    { bind_mode : bind_mode
    ; binding : binding
    }

  and pattern_shape =
    | P_Placeholder
    | P_Ref of pattern
    | P_Unit
    | P_Binding of pattern_binding
    | P_Tuple of pattern_tuple
    | P_Variant of pattern_variant
    | P_Error

  and pattern =
    { shape : pattern_shape
    ; data : ir_data
    }

  (* SCOPE *)
  and interpreter_local =
    { place : place
    ; ty_field : ty_tuple_field
    }

  and interpreter_locals = { by_symbol : interpreter_local SymbolMap.t }

  and interpreter_scope =
    { id : Id.t
    ; depth : int
    ; span : Span.t
    ; mutable locals : interpreter_locals
    ; parent : interpreter_scope option
    ; recursive : bool
    ; mutable closed : bool
    ; mutable on_update : (symbol * (unit -> unit)) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and compiler_scope =
    { id : Id.t
    ; parent : compiler_scope option
    ; recursive : bool
    ; mutable bindings : binding StringMap.t
    ; mutable closed : bool
    ; mutable on_update : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  (* interpreter *)
  and natives =
    { by_name : (ty -> value) StringMap.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and instantiated_generics = { mutable map : value ValueMap.t Id.Map.t }

  and cast_impls =
    { mutable map : value ValueMap.t ValueMap.t
    ; mutable as_module : value ValueMap.t
    }

  and interpreter_state =
    { natives : natives
    ; scope : interpreter_scope
    ; result_scope : var_scope
    ; monomorphization_state : monomorphization_state
    ; mutable contexts : value Id.Map.t
    ; instantiated_generics : instantiated_generics
    ; cast_impls : cast_impls
    ; current_name : name_shape
    }

  (* OTHER *)
  and binding =
    { id : Id.t
    ; scope : var_scope
    ; name : Symbol.t
    ; span : Span.t
    ; ty : ty
    ; label : Label.t
    ; mut : bool
    }

  and ir_data =
    { span : Span.t
    ; ty : ty
    ; compiler_scope : compiler_scope
    ; evaled : ir_evaled
    ; included_file : Uri.t option
    ; id : Id.t
    }

  and ir_evaled =
    { mutable patterns : pattern list
    ; mutable exprs : (expr * value) list
    ; mutable ty_exprs : (ty_expr * ty) list
    ; mutable ty_ascribed : bool
    ; mutable value : value option
    }

  and name = { var : name_shape var }
  and optional_name = { var : name_shape option var }

  and name_shape =
    | Simple of name_part
    | Concat of name_shape * name_part
    | Instantiation of name_instantiation

  and name_instantiation =
    { generic : value
    ; arg : value
    }

  and name_part =
    | Uri of Uri.t
    | Str of string
    | Symbol of Symbol.t

  and ast_data = { span : Span.t } [@@deriving eq, ord]

  type _ compiled_kind =
    | Assignee : assignee_expr compiled_kind
    | Expr : expr compiled_kind
    | PlaceExpr : place_expr compiled_kind
    | TyExpr : ty_expr compiled_kind
    | Pattern : pattern compiled_kind

  and compiled = Compiled : 'a. 'a compiled_kind * 'a -> compiled
end = struct
  (* VAR *)
  type 'a var = ('a, var_scope) Inference.var
  and var_scope = interpreter_scope option

  (* PLACE *)
  and place =
    { id : Id.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mutable state : place_state
    ; ty : ty [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mut : place_mut [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and place_mut =
    | Immutable
    | Mutable
    | Inherit

  and place_state =
    | Uninitialized
    | Occupied of value
    | MovedOut

  (* blocked_value *)
  and blocked_value =
    { shape : blocked_value_shape
    ; ty : ty
    }

  and blocked_value_shape =
    | BV_Binding of binding
    | BV_Instantiate of blocked_value_instantiate
    | BV_ClaimRef of blocked_value
    | BV_FieldRef of blocked_value_field_ref

  and blocked_value_field_ref =
    { obj_ref : blocked_value
    ; member : Tuple.Member.t
    }

  and blocked_value_instantiate =
    { generic : blocked_value
    ; arg : value
    }

  (* VALUE *)
  and value_shape =
    | V_Unit
    | V_Bool of bool
    | V_Int32 of int32
    | V_Int64 of int64
    | V_Float64 of float
    | V_Char of char
    | V_Ref of value_ref
    | V_String of string
    | V_Tuple of value_tuple
    | V_Variant of value_variant
    | V_Ty of ty
    | V_Fn of value_fn
    | V_Generic of value_generic
    | V_NativeFn of value_native_fn
    | V_Ast of Ast.t
    | V_UnwindToken of value_unwind_token
    | V_Target of value_target
    | V_ContextTy of value_context_ty
    | V_CompilerScope of compiler_scope
    | V_Opaque of value_opaque
    | V_Blocked of blocked_value
    | V_Error

  and value_ref =
    { mut : bool
    ; place : place
    }

  and value =
    { var : value_shape var
    ; mutable ty : ty option
    }

  and value_opaque =
    { ty : ty_opaque
    ; value : Obj.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and value_target = { name : string }

  and value_context_ty =
    { id : Id.t
    ; ty : ty
    }

  and value_unwind_token =
    { id : Id.t
    ; result_ty : ty
    }

  and value_fn =
    { ty : ty_fn
    ; fn : value_untyped_fn
    }

  and value_untyped_fn =
    { id : Id.t
    ; def : maybe_compiled_fn
    ; captured : interpreter_scope
    ; monomorphization_state : monomorphization_state
    }

  and monomorphization_state =
    { native : (id, value) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; ty : (id, ty) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; value : (id, value) Hashtbl.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and value_generic =
    { name : name_shape
    ; fn : value_untyped_fn
    ; ty : ty_generic
    }

  and value_tuple_field =
    { place : place
    ; span : Span.t
    ; ty_field : ty_tuple_field
    }

  and value_tuple =
    { ty : ty_tuple
    ; tuple : value_tuple_field Tuple.t
    }

  and value_variant =
    { label : Label.t
    ; data : place option
    ; ty : ty_variant
    }

  and value_native_fn =
    { id : Id.t
    ; name : string
    ; ty : ty_fn
    ; impl : caller:span -> state:interpreter_state -> value -> value
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  (* TY *)
  and ty_tuple_field =
    { ty : ty
    ; symbol : Symbol.t option
    ; label : Label.t option
    }

  and ty_tuple =
    { name : optional_name
    ; tuple : ty_tuple_field Tuple.t
    }

  and ty_fn =
    { arg : ty
    ; result : ty
    }

  and ty_generic =
    { arg : pattern
    ; result : ty
    }

  and ty_unwind_token = { result : ty }
  and ty_variant_data = { data : ty option }

  and ty_variant =
    { name : optional_name
    ; variants : (ty_variant_data, var_scope) Row.t
    }

  and ty_opaque = { name : name }
  and is_mutable = { var : (bool, var_scope) Inference.var }

  and ty_ref =
    { mut : is_mutable
    ; referenced : ty
    }

  and ty_shape =
    | T_Unit
    | T_Bool
    | T_Int32
    | T_Int64
    | T_Float64
    | T_String
    | T_Char
    | T_Ref of ty_ref
    | T_Variant of ty_variant
    | T_Tuple of ty_tuple
    | T_Ty
    | T_Fn of ty_fn
    | T_Generic of ty_generic
    | T_Ast
    | T_UnwindToken of ty_unwind_token
    | T_Target
    | T_ContextTy
    | T_CompilerScope
    | T_Opaque of ty_opaque
    | T_Blocked of blocked_value
    | T_Error

  and ty = { var : ty_shape var }

  (* EXPR *)
  and compiled_fn =
    { arg : pattern
    ; body : expr
    }

  and maybe_compiled_fn =
    { span : Span.t
    ; mutable compiled : compiled_fn option
    ; mutable on_compiled : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and expr_fn =
    { ty : ty_fn
    ; def : maybe_compiled_fn
    }

  and expr_generic =
    { def : maybe_compiled_fn
    ; ty : ty_generic
    }

  and expr_then = { list : expr list }

  and 'a tuple_field_of =
    { label_span : Span.t
    ; label : Label.t option
    ; field : 'a
    }

  and expr_stmt = { expr : expr }

  and 'a tuple_part_of =
    | Field of 'a tuple_field_of
    | Unpack of 'a

  and 'a tuple_of = { parts : 'a tuple_part_of list }
  and expr_tuple = expr tuple_of

  and expr_variant =
    { label : Label.t
    ; label_span : Span.t
    ; value : expr option
    }

  and expr_apply =
    { f : expr
    ; arg : expr
    }

  and expr_instantiate_generic =
    { generic : expr
    ; arg : expr
    }

  and expr_scope = { expr : expr }

  and expr_assign =
    { assignee : assignee_expr
    ; value : place_expr
    }

  and expr_native =
    { id : Id.t
    ; expr : string
    }

  and expr_module =
    { def : expr
    ; bindings : binding list
    }

  and expr_use_dot_star =
    { used : expr
    ; bindings : binding list
    }

  and expr_if =
    { cond : expr
    ; then_case : expr
    ; else_case : expr
    }

  and expr_match_branch =
    { pattern : pattern
    ; body : expr
    }

  and expr_match =
    { value : place_expr
    ; branches : expr_match_branch list
    }

  and expr_quote_ast_child =
    | Group of expr_quote_ast_group
    | Ast of expr

  and expr_quote_ast_group =
    { rule : Syntax.Rule.group option
    ; children : expr_quote_ast_child Tuple.t
    ; span : Span.t
    }

  and expr_quote_ast =
    { rule : Syntax.Rule.t
    ; root : expr_quote_ast_group
    }

  and expr_loop = { body : expr }

  and expr_unwindable =
    { token : pattern
    ; body : expr
    }

  and expr_unwind =
    { token : expr
    ; value : expr
    }

  and expr_target_dependent =
    { branches : expr_target_dependent_branch list
    ; captured : interpreter_scope
    ; mutable interpreter_branch : expr_target_dependent_branch option
    }

  and expr_target_dependent_branch =
    { cond : expr
    ; body : expr
    }

  and expr_inject_context =
    { context_ty : value_context_ty
    ; value : expr
    }

  and expr_current_context = { context_ty : value_context_ty }

  and expr_and =
    { lhs : expr
    ; rhs : expr
    }

  and expr_or =
    { lhs : expr
    ; rhs : expr
    }

  and expr_impl_cast =
    { value : expr
    ; target : value
    ; impl : expr
    }

  and expr_cast =
    { value : expr
    ; target : value
    }

  and expr_ref =
    { mut : bool
    ; place : place_expr
    }

  and expr_constant =
    { id : Id.t
    ; value : value
    }

  and expr_shape =
    | E_Constant of expr_constant
    | E_Ref of expr_ref
    | E_Claim of place_expr
    | E_Then of expr_then
    | E_Stmt of expr_stmt
    | E_Scope of expr_scope
    | E_Fn of expr_fn
    | E_Generic of expr_generic
    | E_Tuple of expr_tuple
    | E_Variant of expr_variant
    | E_Apply of expr_apply
    | E_InstantiateGeneric of expr_instantiate_generic
    | E_Assign of expr_assign
    | E_Ty of ty_expr
    | E_Newtype of ty_expr
    | E_Native of expr_native
    | E_Module of expr_module
    | E_UseDotStar of expr_use_dot_star
    | E_If of expr_if
    | E_And of expr_and
    | E_Or of expr_or
    | E_Match of expr_match
    | E_QuoteAst of expr_quote_ast
    | E_Loop of expr_loop
    | E_Unwindable of expr_unwindable
    | E_Unwind of expr_unwind
    | E_InjectContext of expr_inject_context
    | E_CurrentContext of expr_current_context
    | E_ImplCast of expr_impl_cast
    | E_Cast of expr_cast
    | E_TargetDependent of expr_target_dependent
    | E_Error

  and expr =
    { shape : expr_shape
    ; data : ir_data
    }

  (* PLACE EXPR *)
  and field_expr =
    | Index of int
    | Name of Label.t
    | Expr of expr

  and place_expr_field =
    { obj : place_expr
    ; field : field_expr
    ; field_span : Span.t
    }

  and place_expr_shape =
    | PE_Binding of binding
    | PE_Field of place_expr_field
    | PE_Deref of expr
    | PE_Temp of expr
    | PE_Error

  and place_expr =
    { shape : place_expr_shape
    ; mut : is_mutable
    ; data : ir_data
    }

  (* ASSIGNEE EXPR *)
  and assignee_expr_shape =
    | A_Placeholder
    | A_Unit
    | A_Tuple of assignee_expr_tuple
    | A_Place of place_expr
    | A_Let of pattern
    | A_Error

  and assignee_expr_tuple = assignee_expr tuple_of

  and assignee_expr =
    { shape : assignee_expr_shape
    ; data : ir_data
    }

  (* TYPE EXPR *)
  and ty_expr_fn =
    { arg : ty_expr
    ; result : ty_expr
    }

  and ty_expr_tuple = ty_expr tuple_of
  and ty_expr_union = { elements : ty_expr list }

  and ty_expr_variant_variant =
    { label_span : Span.t
    ; label : Label.t
    ; value : ty_expr option
    }

  and ty_expr_variant = { variants : ty_expr_variant_variant list }

  and ty_expr_ref =
    { mut : is_mutable
    ; referenced : ty_expr
    }

  and ty_expr_shape =
    | TE_Unit
    | TE_Ref of ty_expr_ref
    | TE_Fn of ty_expr_fn
    | TE_Expr of expr
    | TE_Tuple of ty_expr_tuple
    | TE_Variant of ty_expr_variant
    | TE_Union of ty_expr_union
    | TE_Error

  and ty_expr =
    { mutable compiled_shape : ty_expr_shape option
    ; mutable on_compiled : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; (* TODO technically only need span for this? *)
      data : ir_data
    }

  (* PATTERN *)
  and pattern_tuple = pattern tuple_of

  and pattern_variant =
    { label : Label.t
    ; label_span : Span.t
    ; value : pattern option
    }

  and bind_mode =
    | Claim
    | ByRef of { mut : bool }

  and pattern_binding =
    { bind_mode : bind_mode
    ; binding : binding
    }

  and pattern_shape =
    | P_Placeholder
    | P_Ref of pattern
    | P_Unit
    | P_Binding of pattern_binding
    | P_Tuple of pattern_tuple
    | P_Variant of pattern_variant
    | P_Error

  and pattern =
    { shape : pattern_shape
    ; data : ir_data
    }

  (* SCOPE *)
  and interpreter_local =
    { place : place
    ; ty_field : ty_tuple_field
    }

  and interpreter_locals = { by_symbol : interpreter_local SymbolMap.t }

  and interpreter_scope =
    { id : Id.t
    ; depth : int
    ; span : Span.t
    ; mutable locals : interpreter_locals
    ; parent : interpreter_scope option
    ; recursive : bool
    ; mutable closed : bool
    ; mutable on_update : (symbol * (unit -> unit)) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and compiler_scope =
    { id : Id.t
    ; parent : compiler_scope option
    ; recursive : bool
    ; mutable bindings : binding StringMap.t
    ; mutable closed : bool
    ; mutable on_update : (unit -> unit) list
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  (* interpreter *)
  and natives =
    { by_name : (ty -> value) StringMap.t [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and instantiated_generics =
    { mutable map : value ValueMap.t Id.Map.t
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and cast_impls =
    { mutable map : value ValueMap.t ValueMap.t
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mutable as_module : value ValueMap.t
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }

  and interpreter_state =
    { natives : natives
    ; scope : interpreter_scope
    ; result_scope : var_scope
    ; monomorphization_state : monomorphization_state
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    ; mutable contexts : value Id.Map.t
    ; instantiated_generics : instantiated_generics
    ; cast_impls : cast_impls
    ; current_name : name_shape
    }

  (* OTHER *)
  and binding =
    { id : Id.t
    ; scope : var_scope
    ; name : Symbol.t
    ; span : Span.t
    ; ty : ty
    ; label : Label.t
    ; mut : bool
    }

  and ir_data =
    { span : Span.t
    ; ty : ty
    ; compiler_scope : compiler_scope
    ; evaled : ir_evaled
    ; included_file : Uri.t option
    ; id : Id.t
    }

  and ir_evaled =
    { mutable patterns : pattern list
    ; mutable exprs : (expr * value) list
    ; mutable ty_exprs : (ty_expr * ty) list
    ; mutable ty_ascribed : bool
    ; mutable value : value option
    }

  and name = { var : name_shape var }
  and optional_name = { var : name_shape option var }

  and name_shape =
    | Simple of name_part
    | Concat of name_shape * name_part
    | Instantiation of name_instantiation

  and name_instantiation =
    { generic : value
    ; arg : value
    }

  and name_part =
    | Uri of Uri.t
    | Str of string
    | Symbol of Symbol.t

  and ast_data = { span : Span.t } [@@deriving eq, ord]

  type _ compiled_kind =
    | Assignee : assignee_expr compiled_kind
    | Expr : expr compiled_kind
    | PlaceExpr : place_expr compiled_kind
    | TyExpr : ty_expr compiled_kind
    | Pattern : pattern compiled_kind

  and compiled = Compiled : 'a. 'a compiled_kind * 'a -> compiled
end

and ValueImpl : sig
  type t = TypesImpl.value

  val equal : t -> t -> bool
  val compare : t -> t -> int
end = struct
  type t = TypesImpl.value

  module RecurseCache = Inference.CompareRecurseCache

  let equal a b =
    RecurseCache.with_cache (RecurseCache.create ()) (fun () -> TypesImpl.equal_value a b)
  ;;

  let compare a b =
    RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
      TypesImpl.compare_value a b)
  ;;
end

and ValueMap : sig
  type 'a t
  type key = ValueImpl.t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val size : 'a t -> int
end = struct
  type key = ValueImpl.t
  type 'a t = { entries : (key * 'a) list }

  let empty = { entries = [] }

  let find_opt key map =
    map.entries
    |> List.find_map (fun (map_key, value) ->
      if ValueImpl.equal key map_key then Some value else None)
  ;;

  let update (key : key) (f : 'a option -> 'a option) (map : 'a t) : 'a t =
    let existed = ref false in
    let updated_if_existed =
      map.entries
      |> List.filter_map (fun (map_key, current_value) ->
        (if ValueImpl.equal key map_key
         then (
           existed := true;
           f (Some current_value))
         else Some current_value)
        |> Option.map (fun new_value -> map_key, new_value))
    in
    if !existed
    then { entries = updated_if_existed }
    else (
      match f None with
      | None -> map
      | Some value -> { entries = (key, value) :: map.entries })
  ;;

  let add key value map = update key (fun _current -> Some value) map
  let iter f map = map.entries |> List.iter (fun (key, value) -> f key value)

  let union f a b =
    let result = ref a in
    b
    |> iter (fun key value ->
      result
      := !result
         |> update key (fun current_value ->
           match current_value with
           | None -> Some value
           | Some current_value -> f key value current_value));
    !result
  ;;

  let size map = map.entries |> List.length
end

and Ast : (Kast_ast.S with type Data.t = TypesImpl.ast_data) = Kast_ast.Make (AstData)

and AstData : (Kast_ast.DataS with type t = TypesImpl.ast_data) = struct
  type t = TypesImpl.ast_data [@@deriving eq, ord]

  let span ({ span; _ } : t) = span
end

let target_symbol : symbol = Symbol.create "target"

include TypesImpl

type sub_state = interpreter_state

let init_monomorphization_state () : monomorphization_state =
  { native = Hashtbl.create 0; ty = Hashtbl.create 0; value = Hashtbl.create 0 }
;;

let const_shape value : expr_shape = E_Constant { id = Id.gen (); value }
