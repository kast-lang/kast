open Kast_common

module type Deps = sig
  module Pattern : sig
    type t
  end

  module Expr : sig
    type t
  end

  module TyExpr : sig
    type t
  end

  module CompilerScope : sig
    type t
  end

  module Ty : sig
    type t [@@deriving eq, ord]
  end
end

module type S = sig
  module Deps : Deps

  type evaled = {
    mutable patterns : Deps.Pattern.t list;
    mutable exprs : Deps.Expr.t list;
    mutable ty_exprs : Deps.TyExpr.t list;
    mutable ty_ascribed : bool;
  }

  type t = {
    span : Span.t;
    ty : Deps.Ty.t;
    compiler_scope : Deps.CompilerScope.t;
    evaled : evaled;
    included_file : Uri.t option;
  }
  [@@deriving eq, ord]
end

module Make (Deps : Deps) : S = struct
  module Deps = Deps

  type evaled = {
    mutable patterns : Deps.Pattern.t list;
    mutable exprs : Deps.Expr.t list;
    mutable ty_exprs : Deps.TyExpr.t list;
    mutable ty_ascribed : bool;
  }

  type t = {
    span : Span.t;
    ty : Deps.Ty.t;
    compiler_scope : Deps.CompilerScope.t;
    evaled : evaled;
    included_file : Uri.t option;
  }

  let equal a b = Deps.Ty.equal a.ty b.ty
  let compare a b = Deps.Ty.compare a.ty b.ty
end
