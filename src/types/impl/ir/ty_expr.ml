open Kast_common

module Shape = struct
  module type Deps = sig
    module Expr : sig
      type t
    end

    module TyExpr : sig
      type t
    end

    module Unsorted : sig
      type is_mutable
    end
  end

  module type S = sig
    module Deps : Deps

    type fn = {
      arg : Deps.TyExpr.t;
      result : Deps.TyExpr.t;
    }

    and tuple = Deps.TyExpr.t tuple_of
    and union = { elements : Deps.TyExpr.t list }

    and variant_variant = {
      label_span : Span.t;
      label : Label.t;
      value : Deps.TyExpr.t option;
    }

    and variant = { variants : variant_variant list }

    and ref = {
      mut : Deps.Unsorted.is_mutable;
      referenced : Deps.TyExpr.t;
    }

    and t =
      | Unit
      | Ref of ref
      | Fn of fn
      | Expr of Deps.Expr.t
      | Tuple of tuple
      | Variant of variant
      | Union of union
      | Error
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type fn = {
      arg : Deps.TyExpr.t;
      result : Deps.TyExpr.t;
    }

    and tuple = Deps.TyExpr.t tuple_of
    and union = { elements : Deps.TyExpr.t list }

    and variant_variant = {
      label_span : Span.t;
      label : Label.t;
      value : Deps.TyExpr.t option;
    }

    and variant = { variants : variant_variant list }

    and ref = {
      mut : Deps.Unsorted.is_mutable;
      referenced : Deps.TyExpr.t;
    }

    and t =
      | Unit
      | Ref of ref
      | Fn of fn
      | Expr of Deps.Expr.t
      | Tuple of tuple
      | Variant of variant
      | Union of union
      | Error
  end
end

module T = struct
  module type Deps = sig
    module TyExprShape : sig
      type t
    end

    module IrData : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      mutable compiled_shape : Deps.TyExprShape.t option;
      mutable on_compiled : (unit -> unit) list;
      (* TODO technically only need span for this? *)
      data : Deps.IrData.t;
    }
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      mutable compiled_shape : Deps.TyExprShape.t option;
      mutable on_compiled : (unit -> unit) list;
      (* TODO technically only need span for this? *)
      data : Deps.IrData.t;
    }
  end
end

include T
