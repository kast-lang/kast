open Kast_common

module Shape = struct
  module type Deps = sig
    module TyShape : sig
      type fn
      type generic
    end

    module ValueShape : sig
      type context_ty
    end

    module Value : sig
      type t
    end

    module Expr : sig
      type t
    end

    module AssigneeExpr : sig
      type t
    end

    module PlaceExpr : sig
      type t
    end

    module TyExpr : sig
      type t
    end

    module Pattern : sig
      type t
    end

    module Binding : sig
      type t
    end

    module Unsorted : sig
      type maybe_compiled_fn
    end
  end

  module type S = sig
    module Deps : Deps

    type fn = {
      ty : Deps.TyShape.fn;
      def : Deps.Unsorted.maybe_compiled_fn;
    }

    type generic = {
      def : Deps.Unsorted.maybe_compiled_fn;
      ty : Deps.TyShape.generic;
    }

    type then' = { list : Deps.Expr.t list }
    type stmt = { expr : Deps.Expr.t }
    type tuple = Deps.Expr.t tuple_of

    type variant = {
      label : Label.t;
      label_span : Span.t;
      value : Deps.Expr.t option;
    }

    type apply = {
      f : Deps.Expr.t;
      arg : Deps.Expr.t;
    }

    type instantiate_generic = {
      generic : Deps.Expr.t;
      arg : Deps.Expr.t;
    }

    type scope = { expr : Deps.Expr.t }

    type assign = {
      assignee : Deps.AssigneeExpr.t;
      value : Deps.PlaceExpr.t;
    }

    type native = {
      id : Id.t;
      expr : string;
    }

    type module' = { def : Deps.Expr.t }

    type use_dot_star = {
      used : Deps.Expr.t;
      bindings : Deps.Binding.t list;
    }

    type if' = {
      cond : Deps.Expr.t;
      then_case : Deps.Expr.t;
      else_case : Deps.Expr.t;
    }

    type match_branch = {
      pattern : Deps.Pattern.t;
      body : Deps.Expr.t;
    }

    type match' = {
      value : Deps.PlaceExpr.t;
      branches : match_branch list;
    }

    type quote_ast_group = {
      rule : Syntax.Rule.group option;
      children : quote_ast_child Tuple.t;
    }

    and quote_ast_child =
      | Group of quote_ast_group
      | Ast of Deps.Expr.t

    type quote_ast = {
      rule : Syntax.Rule.t;
      root : quote_ast_group;
    }

    type loop = { body : Deps.Expr.t }

    type unwindable = {
      token : Deps.Pattern.t;
      body : Deps.Expr.t;
    }

    type unwind = {
      token : Deps.Expr.t;
      value : Deps.Expr.t;
    }

    type target_dependent_branch = {
      cond : Deps.Expr.t;
      body : Deps.Expr.t;
    }

    type target_dependent = {
      branches : target_dependent_branch list;
      mutable interpreter_branch : target_dependent_branch option;
    }

    type inject_context = {
      context_ty : Deps.ValueShape.context_ty;
      value : Deps.Expr.t;
    }

    type current_context = { context_ty : Deps.ValueShape.context_ty }

    type and' = {
      lhs : Deps.Expr.t;
      rhs : Deps.Expr.t;
    }

    type or' = {
      lhs : Deps.Expr.t;
      rhs : Deps.Expr.t;
    }

    type impl_cast = {
      value : Deps.Expr.t;
      target : Deps.Value.t;
      impl : Deps.Expr.t;
    }

    type cast = {
      value : Deps.Expr.t;
      target : Deps.Value.t;
    }

    type ref = {
      mut : bool;
      place : Deps.PlaceExpr.t;
    }

    type t =
      | Constant of Deps.Value.t
      | Ref of ref
      | Claim of Deps.PlaceExpr.t
      | Then of then'
      | Stmt of stmt
      | Scope of scope
      | Fn of fn
      | Generic of generic
      | Tuple of tuple
      | Variant of variant
      | Apply of apply
      | InstantiateGeneric of instantiate_generic
      | Assign of assign
      | Ty of Deps.TyExpr.t
      | Newtype of Deps.TyExpr.t
      | Native of native
      | Module of module'
      | UseDotStar of use_dot_star
      | If of if'
      | And of and'
      | Or of or'
      | Match of match'
      | QuoteAst of quote_ast
      | Loop of loop
      | Unwindable of unwindable
      | Unwind of unwind
      | InjectContext of inject_context
      | CurrentContext of current_context
      | ImplCast of impl_cast
      | Cast of cast
      | TargetDependent of target_dependent
      | Error
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type fn = {
      ty : Deps.TyShape.fn;
      def : Deps.Unsorted.maybe_compiled_fn;
    }

    type generic = {
      def : Deps.Unsorted.maybe_compiled_fn;
      ty : Deps.TyShape.generic;
    }

    type then' = { list : Deps.Expr.t list }
    type stmt = { expr : Deps.Expr.t }
    type tuple = Deps.Expr.t tuple_of

    type variant = {
      label : Label.t;
      label_span : Span.t;
      value : Deps.Expr.t option;
    }

    type apply = {
      f : Deps.Expr.t;
      arg : Deps.Expr.t;
    }

    type instantiate_generic = {
      generic : Deps.Expr.t;
      arg : Deps.Expr.t;
    }

    type scope = { expr : Deps.Expr.t }

    type assign = {
      assignee : Deps.AssigneeExpr.t;
      value : Deps.PlaceExpr.t;
    }

    type native = {
      id : Id.t;
      expr : string;
    }

    type module' = { def : Deps.Expr.t }

    type use_dot_star = {
      used : Deps.Expr.t;
      bindings : Deps.Binding.t list;
    }

    type if' = {
      cond : Deps.Expr.t;
      then_case : Deps.Expr.t;
      else_case : Deps.Expr.t;
    }

    type match_branch = {
      pattern : Deps.Pattern.t;
      body : Deps.Expr.t;
    }

    type match' = {
      value : Deps.PlaceExpr.t;
      branches : match_branch list;
    }

    type quote_ast_group = {
      rule : Syntax.Rule.group option;
      children : quote_ast_child Tuple.t;
    }

    and quote_ast_child =
      | Group of quote_ast_group
      | Ast of Deps.Expr.t

    type quote_ast = {
      rule : Syntax.Rule.t;
      root : quote_ast_group;
    }

    type loop = { body : Deps.Expr.t }

    type unwindable = {
      token : Deps.Pattern.t;
      body : Deps.Expr.t;
    }

    type unwind = {
      token : Deps.Expr.t;
      value : Deps.Expr.t;
    }

    type target_dependent_branch = {
      cond : Deps.Expr.t;
      body : Deps.Expr.t;
    }

    type target_dependent = {
      branches : target_dependent_branch list;
      mutable interpreter_branch : target_dependent_branch option;
    }

    type inject_context = {
      context_ty : Deps.ValueShape.context_ty;
      value : Deps.Expr.t;
    }

    type current_context = { context_ty : Deps.ValueShape.context_ty }

    type and' = {
      lhs : Deps.Expr.t;
      rhs : Deps.Expr.t;
    }

    type or' = {
      lhs : Deps.Expr.t;
      rhs : Deps.Expr.t;
    }

    type impl_cast = {
      value : Deps.Expr.t;
      target : Deps.Value.t;
      impl : Deps.Expr.t;
    }

    type cast = {
      value : Deps.Expr.t;
      target : Deps.Value.t;
    }

    type ref = {
      mut : bool;
      place : Deps.PlaceExpr.t;
    }

    type t =
      | Constant of Deps.Value.t
      | Ref of ref
      | Claim of Deps.PlaceExpr.t
      | Then of then'
      | Stmt of stmt
      | Scope of scope
      | Fn of fn
      | Generic of generic
      | Tuple of tuple
      | Variant of variant
      | Apply of apply
      | InstantiateGeneric of instantiate_generic
      | Assign of assign
      | Ty of Deps.TyExpr.t
      | Newtype of Deps.TyExpr.t
      | Native of native
      | Module of module'
      | UseDotStar of use_dot_star
      | If of if'
      | And of and'
      | Or of or'
      | Match of match'
      | QuoteAst of quote_ast
      | Loop of loop
      | Unwindable of unwindable
      | Unwind of unwind
      | InjectContext of inject_context
      | CurrentContext of current_context
      | ImplCast of impl_cast
      | Cast of cast
      | TargetDependent of target_dependent
      | Error
  end
end

module T = struct
  module type Deps = sig
    module ExprShape : sig
      type t
    end

    module IrData : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      shape : Deps.ExprShape.t;
      data : Deps.IrData.t;
    }
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      shape : Deps.ExprShape.t;
      data : Deps.IrData.t;
    }
  end
end

include T
