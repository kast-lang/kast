open Kast_common

module Shape = struct
  module type Deps = sig
    module Expr : sig
      type t
    end

    module PlaceExpr : sig
      type t
    end

    module Binding : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type field_expr =
      | Index of int
      | Name of Label.t
      | Expr of Deps.Expr.t

    type field = {
      obj : Deps.PlaceExpr.t;
      field : field_expr;
      field_span : Span.t;
    }

    type t =
      | Binding of Deps.Binding.t
      | Field of field
      | Deref of Deps.Expr.t
      | Temp of Deps.Expr.t
      | Error
  end

  module Make (Deps : Deps) : S with module Deps = Deps = struct
    module Deps = Deps

    type field_expr =
      | Index of int
      | Name of Label.t
      | Expr of Deps.Expr.t

    type field = {
      obj : Deps.PlaceExpr.t;
      field : field_expr;
      field_span : Span.t;
    }

    type t =
      | Binding of Deps.Binding.t
      | Field of field
      | Deref of Deps.Expr.t
      | Temp of Deps.Expr.t
      | Error
  end
end

module T = struct
  module type Deps = sig
    module PlaceExprShape : sig
      type t
    end

    module IrData : sig
      type t
    end

    module Unsorted : sig
      type is_mutable
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      shape : Deps.PlaceExprShape.t;
      mut : Deps.Unsorted.is_mutable;
      data : Deps.IrData.t;
    }
  end

  module Make (Deps : Deps) : S with module Deps = Deps = struct
    module Deps = Deps

    type t = {
      shape : Deps.PlaceExprShape.t;
      mut : Deps.Unsorted.is_mutable;
      data : Deps.IrData.t;
    }
  end
end

include T
