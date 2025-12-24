open Std
open Kast_util
module Ast = Kast_ast
module Syntax = Kast_syntax
module Inference = Kast_inference_base
module Label = Label

type 'a tuple_part_of =
  | Field of 'a tuple_field_of
  | Unpack of 'a

and 'a tuple_of = { parts : 'a tuple_part_of list }

and 'a tuple_field_of = {
  label_span : Span.t;
  label : Label.t option;
  field : 'a;
}

module rec Unused : sig end = struct end

and BlockedValueShape : sig
  type blocked_value_field_ref = {
    obj_ref : BlockedValue.t;
    member : Tuple.Member.t;
  }
  [@@deriving eq, ord]

  type blocked_value_instantiate = {
    generic : BlockedValue.t;
    arg : Value.t;
  }
  [@@deriving eq, ord]

  type t =
    | BV_Binding of Binding.t
    | BV_Instantiate of blocked_value_instantiate
    | BV_ClaimRef of BlockedValue.t
    | BV_FieldRef of blocked_value_field_ref
  [@@deriving eq, ord]
end = struct
  type blocked_value_field_ref = {
    obj_ref : BlockedValue.t;
    member : Tuple.Member.t;
  }
  [@@deriving eq, ord]

  type blocked_value_instantiate = {
    generic : BlockedValue.t;
    arg : Value.t;
  }
  [@@deriving eq, ord]

  type t =
    | BV_Binding of Binding.t
    | BV_Instantiate of blocked_value_instantiate
    | BV_ClaimRef of BlockedValue.t
    | BV_FieldRef of blocked_value_field_ref
  [@@deriving eq, ord]
end

and BlockedValue : sig
  type t = {
    shape : BlockedValueShape.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]
end = struct
  type t = {
    shape : BlockedValueShape.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]
end

and ValueShape : sig
  type value_ref = {
    mut : bool;
    place : Place.t;
  }
  [@@deriving eq, ord]

  type value_opaque = {
    ty : TyShape.ty_opaque;
    value : Obj.t; [@equal Repr.equal] [@compare Stdlib.compare]
  }
  [@@deriving eq, ord]

  type value_target = { name : string } [@@deriving eq, ord]

  type value_context_ty = {
    id : Id.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]

  type value_unwind_token = {
    id : Id.t;
    result_ty : Ty.t;
  }
  [@@deriving eq, ord]

  type value_untyped_fn = {
    id : Id.t;
    def : Unsorted.maybe_compiled_fn;
    calculated_natives : (id, Value.t) Hashtbl.t;
    captured : InterpreterScope.t;
  }
  [@@deriving eq, ord]

  type value_fn = {
    ty : TyShape.ty_fn;
    fn : value_untyped_fn;
  }
  [@@deriving eq, ord]

  type value_generic = {
    id : Id.t;
    name : NameShape.t;
    fn : value_untyped_fn;
    ty : TyShape.ty_generic;
  }
  [@@deriving eq, ord]

  type value_tuple_field = {
    place : Place.t;
    span : Span.t;
    ty_field : TyShape.ty_tuple_field;
  }
  [@@deriving eq, ord]

  type value_tuple = {
    ty : TyShape.ty_tuple;
    tuple : value_tuple_field Tuple.t;
  }
  [@@deriving eq, ord]

  type value_variant = {
    label : Label.t;
    data : Place.t option;
    ty : TyShape.ty_variant;
  }
  [@@deriving eq, ord]

  type value_native_fn = {
    id : Id.t;
    name : string;
    ty : TyShape.ty_fn;
    impl : caller:span -> state:Interpreter.state -> Value.t -> Value.t;
  }
  [@@deriving eq, ord]

  type t =
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
    | V_Ty of Ty.t
    | V_Fn of value_fn
    | V_Generic of value_generic
    | V_NativeFn of value_native_fn
    | V_Ast of Ast.t
    | V_UnwindToken of value_unwind_token
    | V_Target of value_target
    | V_ContextTy of value_context_ty
    | V_CompilerScope of CompilerScope.t
    | V_Opaque of value_opaque
    | V_Blocked of BlockedValue.t
    | V_Error
  [@@deriving eq, ord]

  module Scope : Inference.Scope

  val scope : t -> Scope.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val unite : t Inference.unite
  val error : unit -> t
end = struct
  type value_ref = {
    mut : bool;
    place : Place.t;
  }
  [@@deriving eq, ord]

  type value_opaque = {
    ty : TyShape.ty_opaque;
    value : Obj.t; [@equal Stdlib.( = )] [@compare Stdlib.compare]
  }
  [@@deriving eq, ord]

  type value_target = { name : string } [@@deriving eq, ord]

  type value_context_ty = {
    id : Id.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]

  type value_unwind_token = {
    id : Id.t;
    result_ty : Ty.t;
  }
  [@@deriving eq, ord]

  type value_untyped_fn = {
    id : Id.t;
    def : Unsorted.maybe_compiled_fn;
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    calculated_natives : (id, Value.t) Hashtbl.t;
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    captured : InterpreterScope.t;
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
  }
  [@@deriving eq, ord]

  type value_fn = {
    ty : TyShape.ty_fn;
    fn : value_untyped_fn;
  }
  [@@deriving eq, ord]

  type value_generic = {
    id : Id.t;
    name : NameShape.t;
    fn : value_untyped_fn;
    ty : TyShape.ty_generic;
  }
  [@@deriving eq, ord]

  type value_tuple_field = {
    place : Place.t;
    span : Span.t;
    ty_field : TyShape.ty_tuple_field;
  }
  [@@deriving eq, ord]

  type value_tuple = {
    ty : TyShape.ty_tuple;
    tuple : value_tuple_field Tuple.t;
  }
  [@@deriving eq, ord]

  type value_variant = {
    label : Label.t;
    data : Place.t option;
    ty : TyShape.ty_variant;
  }
  [@@deriving eq, ord]

  type value_native_fn = {
    id : Id.t;
    name : string;
    ty : TyShape.ty_fn;
    impl : caller:span -> state:Interpreter.state -> Value.t -> Value.t;
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
  }
  [@@deriving eq, ord]

  type t =
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
    | V_Ty of Ty.t
    | V_Fn of value_fn
    | V_Generic of value_generic
    | V_NativeFn of value_native_fn
    | V_Ast of Ast.t
    | V_UnwindToken of value_unwind_token
    | V_Target of value_target
    | V_ContextTy of value_context_ty
    | V_CompilerScope of CompilerScope.t
    | V_Opaque of value_opaque
    | V_Blocked of BlockedValue.t
    | V_Error
  [@@deriving eq, ord]

  module Scope = VarScope

  let scope _ = failwith __LOC__
  let error = V_Error
end

and ValueVar : Inference.Var.S = Inference.Var.Make (ValueShape)

and Value : sig
  type t = {
    var : ValueVar.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]
end = struct
  type t = {
    var : ValueVar.t;
    ty : Ty.t;
  }
  [@@deriving eq, ord]
end

and TyShape : sig
  type ty_tuple_field = {
    ty : Ty.t;
    label : Label.t option;
  }
  [@@deriving eq, ord]

  type ty_tuple = {
    name : OptionalName.t;
    tuple : ty_tuple_field Tuple.t;
  }
  [@@deriving eq, ord]

  type ty_fn = {
    arg : Ty.t;
    result : Ty.t;
  }
  [@@deriving eq, ord]

  type ty_generic = {
    arg : Pattern.t;
    result : Ty.t;
  }
  [@@deriving eq, ord]

  type ty_unwind_token = { result : Ty.t } [@@deriving eq, ord]
  type ty_variant_data = { data : Ty.t option } [@@deriving eq, ord]

  type ty_variant = {
    name : OptionalName.t;
    variants : TyVariantRow.t;
  }
  [@@deriving eq, ord]

  type ty_opaque = { name : Name.t } [@@deriving eq, ord]

  type ty_ref = {
    mut : Unsorted.is_mutable;
    referenced : Ty.t;
  }
  [@@deriving eq, ord]

  type t =
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
    | T_Blocked of BlockedValue.t
    | T_Error
  [@@deriving eq, ord]
end = struct
  type ty_tuple_field = {
    ty : Ty.t;
    label : Label.t option;
  }
  [@@deriving eq, ord]

  type ty_tuple = {
    name : OptionalName.t;
    tuple : ty_tuple_field Tuple.t;
  }
  [@@deriving eq, ord]

  type ty_fn = {
    arg : Ty.t;
    result : Ty.t;
  }
  [@@deriving eq, ord]

  type ty_generic = {
    arg : Pattern.t;
    result : Ty.t;
  }
  [@@deriving eq, ord]

  type ty_unwind_token = { result : Ty.t } [@@deriving eq, ord]
  type ty_variant_data = { data : Ty.t option } [@@deriving eq, ord]

  type ty_variant = {
    name : OptionalName.t;
    variants : TyVariantRow.t;
  }
  [@@deriving eq, ord]

  type ty_opaque = { name : Name.t } [@@deriving eq, ord]

  type ty_ref = {
    mut : Unsorted.is_mutable;
    referenced : Ty.t;
  }
  [@@deriving eq, ord]

  type t =
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
    | T_Blocked of BlockedValue.t
    | T_Error
  [@@deriving eq, ord]
end

and BoolVar : Inference.Var.S = Inference.Var.Make (Bool)
and TyVariantData : sig end = struct end
and TyVariantRow : Row.S = Row.Make (TyVariantData)
and TyVar : Inference.Var.S = Inference.Var.Make (TyShape)

and Ty : sig
  type t = { var : TyVar.t } [@@deriving eq, ord]
end = struct
  type t = { var : TyVar.t } [@@deriving eq, ord]
end

and Place : sig
  type mut =
    | Immutable
    | Mutable
    | Inherit
  [@@deriving eq, ord]

  type state =
    | Uninitialized
    | Occupied of Value.t
    | MovedOut
  [@@deriving eq, ord]

  type t = {
    id : Id.t;
    mutable state : state;
    ty : Ty.t;
    mut : mut;
  }
  [@@deriving eq, ord]
end = struct
  type mut =
    | Immutable
    | Mutable
    | Inherit
  [@@deriving eq, ord]

  type state =
    | Uninitialized
    | Occupied of Value.t
    | MovedOut
  [@@deriving eq, ord]

  type t = {
    id : Id.t;
    mutable state : state;
    ty : Ty.t;
    mut : mut;
  }
  [@@deriving eq, ord]
end

and ExprShape : sig
  type expr_fn = {
    ty : TyShape.ty_fn;
    def : Unsorted.maybe_compiled_fn;
  }

  and expr_generic = {
    def : Unsorted.maybe_compiled_fn;
    ty : TyShape.ty_generic;
  }

  and expr_then = { list : Expr.t list }
  and expr_stmt = { expr : Expr.t }
  and expr_tuple = Expr.t tuple_of

  and expr_variant = {
    label : Label.t;
    label_span : Span.t;
    value : Expr.t option;
  }

  and expr_apply = {
    f : Expr.t;
    arg : Expr.t;
  }

  and expr_instantiate_generic = {
    generic : Expr.t;
    arg : Expr.t;
  }

  and expr_scope = { expr : Expr.t }

  and expr_assign = {
    assignee : AssigneeExpr.t;
    value : PlaceExpr.t;
  }

  and expr_native = {
    id : Id.t;
    expr : string;
  }

  and expr_module = { def : Expr.t }

  and expr_use_dot_star = {
    used : Expr.t;
    bindings : Binding.t list;
  }

  and expr_if = {
    cond : Expr.t;
    then_case : Expr.t;
    else_case : Expr.t;
  }

  and expr_match_branch = {
    pattern : Pattern.t;
    body : Expr.t;
  }

  and expr_match = {
    value : PlaceExpr.t;
    branches : expr_match_branch list;
  }

  and expr_quote_ast_child =
    | Group of expr_quote_ast_group
    | Ast of Expr.t

  and expr_quote_ast_group = {
    rule : Syntax.Rule.group option;
    children : expr_quote_ast_child Tuple.t;
  }

  and expr_quote_ast = {
    rule : Syntax.Rule.t;
    root : expr_quote_ast_group;
  }

  and expr_loop = { body : Expr.t }

  and expr_unwindable = {
    token : Pattern.t;
    body : Expr.t;
  }

  and expr_unwind = {
    token : Expr.t;
    value : Expr.t;
  }

  and expr_target_dependent = {
    branches : expr_target_dependent_branch list;
    mutable interpreter_branch : expr_target_dependent_branch option;
  }

  and expr_target_dependent_branch = {
    cond : Expr.t;
    body : Expr.t;
  }

  and expr_inject_context = {
    context_ty : ValueShape.value_context_ty;
    value : Expr.t;
  }

  and expr_current_context = { context_ty : ValueShape.value_context_ty }

  and expr_and = {
    lhs : Expr.t;
    rhs : Expr.t;
  }

  and expr_or = {
    lhs : Expr.t;
    rhs : Expr.t;
  }

  and expr_impl_cast = {
    value : Expr.t;
    target : Value.t;
    impl : Expr.t;
  }

  and expr_cast = {
    value : Expr.t;
    target : Value.t;
  }

  and expr_ref = {
    mut : bool;
    place : PlaceExpr.t;
  }

  and t =
    | E_Constant of Value.t
    | E_Ref of expr_ref
    | E_Claim of PlaceExpr.t
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
    | E_Ty of TyExpr.t
    | E_Newtype of TyExpr.t
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
end = struct
  type expr_fn = {
    ty : TyShape.ty_fn;
    def : Unsorted.maybe_compiled_fn;
  }

  and expr_generic = {
    def : Unsorted.maybe_compiled_fn;
    ty : TyShape.ty_generic;
  }

  and expr_then = { list : Expr.t list }
  and expr_stmt = { expr : Expr.t }
  and expr_tuple = Expr.t tuple_of

  and expr_variant = {
    label : Label.t;
    label_span : Span.t;
    value : Expr.t option;
  }

  and expr_apply = {
    f : Expr.t;
    arg : Expr.t;
  }

  and expr_instantiate_generic = {
    generic : Expr.t;
    arg : Expr.t;
  }

  and expr_scope = { expr : Expr.t }

  and expr_assign = {
    assignee : AssigneeExpr.t;
    value : PlaceExpr.t;
  }

  and expr_native = {
    id : Id.t;
    expr : string;
  }

  and expr_module = { def : Expr.t }

  and expr_use_dot_star = {
    used : Expr.t;
    bindings : Binding.t list;
  }

  and expr_if = {
    cond : Expr.t;
    then_case : Expr.t;
    else_case : Expr.t;
  }

  and expr_match_branch = {
    pattern : Pattern.t;
    body : Expr.t;
  }

  and expr_match = {
    value : PlaceExpr.t;
    branches : expr_match_branch list;
  }

  and expr_quote_ast_child =
    | Group of expr_quote_ast_group
    | Ast of Expr.t

  and expr_quote_ast_group = {
    rule : Syntax.Rule.group option;
    children : expr_quote_ast_child Tuple.t;
  }

  and expr_quote_ast = {
    rule : Syntax.Rule.t;
    root : expr_quote_ast_group;
  }

  and expr_loop = { body : Expr.t }

  and expr_unwindable = {
    token : Pattern.t;
    body : Expr.t;
  }

  and expr_unwind = {
    token : Expr.t;
    value : Expr.t;
  }

  and expr_target_dependent = {
    branches : expr_target_dependent_branch list;
    mutable interpreter_branch : expr_target_dependent_branch option;
  }

  and expr_target_dependent_branch = {
    cond : Expr.t;
    body : Expr.t;
  }

  and expr_inject_context = {
    context_ty : ValueShape.value_context_ty;
    value : Expr.t;
  }

  and expr_current_context = { context_ty : ValueShape.value_context_ty }

  and expr_and = {
    lhs : Expr.t;
    rhs : Expr.t;
  }

  and expr_or = {
    lhs : Expr.t;
    rhs : Expr.t;
  }

  and expr_impl_cast = {
    value : Expr.t;
    target : Value.t;
    impl : Expr.t;
  }

  and expr_cast = {
    value : Expr.t;
    target : Value.t;
  }

  and expr_ref = {
    mut : bool;
    place : PlaceExpr.t;
  }

  and t =
    | E_Constant of Value.t
    | E_Ref of expr_ref
    | E_Claim of PlaceExpr.t
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
    | E_Ty of TyExpr.t
    | E_Newtype of TyExpr.t
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
end

and Expr : sig
  type t = {
    shape : ExprShape.t;
    data : IrData.t;
  }
end = struct
  type t = {
    shape : ExprShape.t;
    data : IrData.t;
  }
end

and PlaceExprShape : sig
  type field_expr =
    | Index of int
    | Name of Label.t
    | Expr of Expr.t

  type place_expr_field = {
    obj : PlaceExpr.t;
    field : field_expr;
    field_span : Span.t;
  }

  type t =
    | PE_Binding of Binding.t
    | PE_Field of place_expr_field
    | PE_Deref of Expr.t
    | PE_Temp of Expr.t
    | PE_Error
end = struct
  type field_expr =
    | Index of int
    | Name of Label.t
    | Expr of Expr.t

  type place_expr_field = {
    obj : PlaceExpr.t;
    field : field_expr;
    field_span : Span.t;
  }

  type t =
    | PE_Binding of Binding.t
    | PE_Field of place_expr_field
    | PE_Deref of Expr.t
    | PE_Temp of Expr.t
    | PE_Error
end

and PlaceExpr : sig
  type t = {
    shape : PlaceExprShape.t;
    mut : Unsorted.is_mutable;
    data : IrData.t;
  }
end = struct
  type t = {
    shape : PlaceExprShape.t;
    mut : Unsorted.is_mutable;
    data : IrData.t;
  }
end

and AssigneeExprShape : sig
  type assignee_expr_tuple = AssigneeExpr.t tuple_of

  type t =
    | A_Placeholder
    | A_Unit
    | A_Tuple of assignee_expr_tuple
    | A_Place of PlaceExpr.t
    | A_Let of Pattern.t
    | A_Error
end = struct
  type assignee_expr_tuple = AssigneeExpr.t tuple_of

  type t =
    | A_Placeholder
    | A_Unit
    | A_Tuple of assignee_expr_tuple
    | A_Place of PlaceExpr.t
    | A_Let of Pattern.t
    | A_Error
end

and AssigneeExpr : sig
  type t = {
    shape : AssigneeExprShape.t;
    data : IrData.t;
  }
end = struct
  type t = {
    shape : AssigneeExprShape.t;
    data : IrData.t;
  }
end

and TyExprShape : sig
  type ty_expr_fn = {
    arg : TyExpr.t;
    result : TyExpr.t;
  }

  and ty_expr_tuple = TyExpr.t tuple_of
  and ty_expr_union = { elements : TyExpr.t list }

  and ty_expr_variant_variant = {
    label_span : Span.t;
    label : Label.t;
    value : TyExpr.t option;
  }

  and ty_expr_variant = { variants : ty_expr_variant_variant list }

  and ty_expr_ref = {
    mut : Unsorted.is_mutable;
    referenced : TyExpr.t;
  }

  and t =
    | TE_Unit
    | TE_Ref of ty_expr_ref
    | TE_Fn of ty_expr_fn
    | TE_Expr of Expr.t
    | TE_Tuple of ty_expr_tuple
    | TE_Variant of ty_expr_variant
    | TE_Union of ty_expr_union
    | TE_Error
end = struct
  type ty_expr_fn = {
    arg : TyExpr.t;
    result : TyExpr.t;
  }

  and ty_expr_tuple = TyExpr.t tuple_of
  and ty_expr_union = { elements : TyExpr.t list }

  and ty_expr_variant_variant = {
    label_span : Span.t;
    label : Label.t;
    value : TyExpr.t option;
  }

  and ty_expr_variant = { variants : ty_expr_variant_variant list }

  and ty_expr_ref = {
    mut : Unsorted.is_mutable;
    referenced : TyExpr.t;
  }

  and t =
    | TE_Unit
    | TE_Ref of ty_expr_ref
    | TE_Fn of ty_expr_fn
    | TE_Expr of Expr.t
    | TE_Tuple of ty_expr_tuple
    | TE_Variant of ty_expr_variant
    | TE_Union of ty_expr_union
    | TE_Error
end

and TyExpr : sig
  type t = {
    mutable compiled_shape : TyExprShape.t option;
    mutable on_compiled : (unit -> unit) list;
    (* TODO technically only need span for this? *)
    data : IrData.t;
  }
end = struct
  type t = {
    mutable compiled_shape : TyExprShape.t option;
    mutable on_compiled : (unit -> unit) list;
    (* TODO technically only need span for this? *)
    data : IrData.t;
  }
end

and PatternShape : sig
  type pattern_tuple = Pattern.t tuple_of

  and pattern_variant = {
    label : Label.t;
    label_span : Span.t;
    value : Pattern.t option;
  }

  and pattern_binding = {
    by_ref : bool;
    binding : Binding.t;
  }

  and t =
    | P_Placeholder
    | P_Ref of Pattern.t
    | P_Unit
    | P_Binding of pattern_binding
    | P_Tuple of pattern_tuple
    | P_Variant of pattern_variant
    | P_Error
end = struct
  type pattern_tuple = Pattern.t tuple_of

  and pattern_variant = {
    label : Label.t;
    label_span : Span.t;
    value : Pattern.t option;
  }

  and pattern_binding = {
    by_ref : bool;
    binding : Binding.t;
  }

  and t =
    | P_Placeholder
    | P_Ref of Pattern.t
    | P_Unit
    | P_Binding of pattern_binding
    | P_Tuple of pattern_tuple
    | P_Variant of pattern_variant
    | P_Error
end

and Pattern : sig
  type t = {
    shape : PatternShape.t;
    data : IrData.t;
  }
end = struct
  type t = {
    shape : PatternShape.t;
    data : IrData.t;
  }
end

and InterpreterScope : sig
  type interpreter_local = {
    place : Place.t;
    ty_field : TyShape.ty_tuple_field;
  }

  and interpreter_locals = { by_symbol : interpreter_local SymbolMap.t }

  and t = {
    id : Id.t;
    span : Span.t;
    mutable locals : interpreter_locals;
    parent : t option;
    recursive : bool;
    mutable closed : bool;
    mutable on_update : (symbol * (unit -> unit)) list;
  }
end = struct
  type interpreter_local = {
    place : Place.t;
    ty_field : TyShape.ty_tuple_field;
  }

  and interpreter_locals = { by_symbol : interpreter_local SymbolMap.t }

  and t = {
    id : Id.t;
    span : Span.t;
    mutable locals : interpreter_locals;
    parent : t option;
    recursive : bool;
    mutable closed : bool;
    mutable on_update : (symbol * (unit -> unit)) list;
  }
end

and VarScope : Inference.Scope = struct
  type t = InterpreterScope.t option

  let root () = None

  let common a b =
    match (a, b) with
    | None, None | Some _, None | None, Some _ -> None
    | Some a, Some b ->
        (* TODO calculate LCA *)
        failwith __LOC__
end

and CompilerScope : sig
  type t = {
    id : Id.t;
    parent : t option;
    recursive : bool;
    mutable bindings : Binding.t StringMap.t;
    mutable closed : bool;
    mutable on_update : (unit -> unit) list;
  }
  [@@deriving eq, ord]
end = struct
  type t = {
    id : Id.t;
    parent : t option;
    recursive : bool;
    mutable bindings : Binding.t StringMap.t;
    mutable closed : bool;
    mutable on_update : (unit -> unit) list;
  }

  let equal a b = Id.equal a.id b.id
  let compare a b = Id.compare a.id b.id
end

and Interpreter : sig
  type natives = { by_name : (Ty.t -> Value.t) StringMap.t }
  and instantiated_generics = { mutable map : Value.t ValueMap.t Id.Map.t }

  and cast_impls = {
    mutable map : Value.t ValueMap.t ValueMap.t;
    mutable as_module : Value.t ValueMap.t;
  }

  and state = {
    natives : natives;
    scope : InterpreterScope.t;
    current_fn_natives : (id, Value.t) Hashtbl.t;
    mutable contexts : Value.t Id.Map.t;
    instantiated_generics : instantiated_generics;
    cast_impls : cast_impls;
    current_name : NameShape.t;
  }
end = struct
  type natives = { by_name : (Ty.t -> Value.t) StringMap.t }
  and instantiated_generics = { mutable map : Value.t ValueMap.t Id.Map.t }

  and cast_impls = {
    mutable map : Value.t ValueMap.t ValueMap.t;
    mutable as_module : Value.t ValueMap.t;
  }

  and state = {
    natives : natives;
    scope : InterpreterScope.t;
    current_fn_natives : (id, Value.t) Hashtbl.t;
    mutable contexts : Value.t Id.Map.t;
    instantiated_generics : instantiated_generics;
    cast_impls : cast_impls;
    current_name : NameShape.t;
  }
end

and Binding : sig
  type t = {
    id : Id.t;
    name : Symbol.t;
    span : Span.t;
    ty : Ty.t;
    label : Label.t;
    mut : bool;
  }
  [@@deriving eq, ord]
end = struct
  type t = {
    id : Id.t;
    name : Symbol.t;
    span : Span.t;
    ty : Ty.t;
    label : Label.t;
    mut : bool;
  }

  let equal a b = Id.equal a.id b.id
  let compare a b = Id.compare a.id b.id
end

and IrData : sig
  type evaled = {
    mutable patterns : Pattern.t list;
    mutable exprs : Expr.t list;
    mutable ty_exprs : TyExpr.t list;
    mutable ty_ascribed : bool;
  }

  type t = {
    span : Span.t;
    ty : Ty.t;
    compiler_scope : CompilerScope.t;
    evaled : evaled;
    included_file : Uri.t option;
  }
end = struct
  type evaled = {
    mutable patterns : Pattern.t list;
    mutable exprs : Expr.t list;
    mutable ty_exprs : TyExpr.t list;
    mutable ty_ascribed : bool;
  }

  type t = {
    span : Span.t;
    ty : Ty.t;
    compiler_scope : CompilerScope.t;
    evaled : evaled;
    included_file : Uri.t option;
  }
end

and NameShape : sig
  type part =
    | Uri of Uri.t
    | Str of string
    | Symbol of Symbol.t
  [@@deriving eq, ord]

  type instantiation = {
    generic : Value.t;
    arg : Value.t;
  }
  [@@deriving eq, ord]

  type t =
    | Simple of part
    | Concat of t * part
    | Instantiation of instantiation
  [@@deriving eq, ord]
end = struct
  type part =
    | Uri of Uri.t
    | Str of string
    | Symbol of Symbol.t
  [@@deriving eq, ord]

  type instantiation = {
    generic : Value.t;
    arg : Value.t;
  }
  [@@deriving eq, ord]

  type t =
    | Simple of part
    | Concat of t * part
    | Instantiation of instantiation
  [@@deriving eq, ord]
end

and NameVar : Inference.Var.S = Inference.Var.Make (NameShape)

and Name : sig
  type t = { var : NameVar.t } [@@deriving eq, ord]
end = struct
  type t = { var : NameVar.t } [@@deriving eq, ord]
end

and OptionalNameShape : sig
  type t = NameShape.t option
end = struct
  type t = NameShape.t option
end

and OptionalNameShapeVar : Inference.Var.S =
  Inference.Var.Make (OptionalNameShape)

and OptionalName : sig
  type t = { var : OptionalNameShapeVar.t }
end = struct
  type t = { var : OptionalNameShapeVar.t }
end

and Compiled : sig
  type _ kind =
    | Assignee : AssigneeExpr.t kind
    | Expr : Expr.t kind
    | PlaceExpr : PlaceExpr.t kind
    | TyExpr : TyExpr.t kind
    | Pattern : Pattern.t kind

  type t = Compiled : 'a. 'a kind * 'a -> t
end = struct
  type _ kind =
    | Assignee : AssigneeExpr.t kind
    | Expr : Expr.t kind
    | PlaceExpr : PlaceExpr.t kind
    | TyExpr : TyExpr.t kind
    | Pattern : Pattern.t kind

  type t = Compiled : 'a. 'a kind * 'a -> t
end

and ValueMap : sig
  type 'a t
  type key = Value.t

  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
end = struct
  type key = Value.t
  type 'a t = { entries : (key * 'a) list }

  let empty = { entries = [] }
  let add key value map = { entries = (key, value) :: map.entries }

  let find_opt key map =
    map.entries
    |> List.find_map (fun (map_key, value) ->
        if ValueImpl.equal key map_key then Some value else None)

  let update (key : key) (f : 'a option -> 'a option) (map : 'a t) : 'a t =
    let existed = ref false in
    let updated_if_existed =
      map.entries
      |> List.filter_map (fun (map_key, current_value) ->
          (if ValueImpl.equal key map_key then (
             existed := true;
             f (Some current_value))
           else Some current_value)
          |> Option.map (fun new_value -> (map_key, new_value)))
    in
    if !existed then { entries = updated_if_existed }
    else
      match f None with
      | None -> map
      | Some value -> { entries = (key, value) :: map.entries }

  let iter f map = map.entries |> List.iter (fun (key, value) -> f key value)

  let union f a b =
    let result = ref a in
    b
    |> iter (fun key value ->
        result :=
          !result
          |> update key (fun current_value ->
              match current_value with
              | None -> Some value
              | Some current_value -> f key value current_value));
    !result
end

and Unsorted : sig
  type compiled_fn = {
    arg : Pattern.t;
    body : Expr.t;
    evaled_result : TyExpr.t option;
  }

  and maybe_compiled_fn = {
    mutable compiled : compiled_fn option;
    mutable on_compiled : (unit -> unit) list;
  }

  type is_mutable = { var : BoolVar.t }
end = struct
  type compiled_fn = {
    arg : Pattern.t;
    body : Expr.t;
    evaled_result : TyExpr.t option;
  }

  and maybe_compiled_fn = {
    mutable compiled : compiled_fn option;
    mutable on_compiled : (unit -> unit) list;
  }

  type is_mutable = { var : BoolVar.t }
end
