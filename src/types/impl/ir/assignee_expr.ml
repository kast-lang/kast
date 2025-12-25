open Kast_common

module Shape = struct
  module type Deps = sig
    module AssigneeExpr : sig
      type t
    end

    module PlaceExpr : sig
      type t
    end

    module Pattern : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type tuple = Deps.AssigneeExpr.t tuple_of

    type t =
      | Placeholder
      | Unit
      | Tuple of tuple
      | Place of Deps.PlaceExpr.t
      | Let of Deps.Pattern.t
      | Error
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type tuple = Deps.AssigneeExpr.t tuple_of

    type t =
      | Placeholder
      | Unit
      | Tuple of tuple
      | Place of Deps.PlaceExpr.t
      | Let of Deps.Pattern.t
      | Error
  end
end

module T = struct
  module type Deps = sig
    module AssigneeExprShape : sig
      type t
    end

    module IrData : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      shape : Deps.AssigneeExprShape.t;
      data : Deps.IrData.t;
    }
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      shape : Deps.AssigneeExprShape.t;
      data : Deps.IrData.t;
    }
  end
end

include T
