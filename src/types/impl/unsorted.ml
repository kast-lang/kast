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
end

module type S = sig
  module Deps : Deps

  type compiled_fn = {
    arg : Deps.Pattern.t;
    body : Deps.Expr.t;
    evaled_result_ty : Deps.TyExpr.t option;
  }

  type maybe_compiled_fn = {
    mutable compiled : compiled_fn option;
    mutable on_compiled : (unit -> unit) list;
  }

  type is_mutable = { var : BoolVar.t } [@@deriving eq, ord]
end

module Make (Deps : Deps) : S = struct
  module Deps = Deps

  type compiled_fn = {
    arg : Deps.Pattern.t;
    body : Deps.Expr.t;
    evaled_result_ty : Deps.TyExpr.t option;
  }

  type maybe_compiled_fn = {
    mutable compiled : compiled_fn option;
    mutable on_compiled : (unit -> unit) list;
  }

  type is_mutable = { var : BoolVar.t } [@@deriving eq, ord]
end
