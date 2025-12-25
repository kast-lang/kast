open Kast_common

module Shape = struct
  module type Deps = sig
    module Value : sig
      type t [@@deriving eq, ord]
    end

    module BlockedValue : sig
      type t [@@deriving eq, ord]
    end

    module Binding : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type field_ref = {
      obj_ref : Deps.BlockedValue.t;
      member : Tuple.Member.t;
    }
    [@@deriving eq, ord]

    type instantiate = {
      generic : Deps.BlockedValue.t;
      arg : Deps.Value.t;
    }
    [@@deriving eq, ord]

    type t =
      | Binding of Deps.Binding.t
      | Instantiate of instantiate
      | ClaimRef of Deps.BlockedValue.t
      | FieldRef of field_ref
    [@@deriving eq, ord]
  end

  module Make (Deps : Deps) = struct
    module Deps = Deps

    type field_ref = {
      obj_ref : Deps.BlockedValue.t;
      member : Tuple.Member.t;
    }
    [@@deriving eq, ord]

    type instantiate = {
      generic : Deps.BlockedValue.t;
      arg : Deps.Value.t;
    }
    [@@deriving eq, ord]

    type t =
      | Binding of Deps.Binding.t
      | Instantiate of instantiate
      | ClaimRef of Deps.BlockedValue.t
      | FieldRef of field_ref
    [@@deriving eq, ord]
  end
end

module T = struct
  module type Deps = sig
    module BlockedValueShape : sig
      type t [@@deriving eq, ord]
    end

    module Ty : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      shape : Deps.BlockedValueShape.t;
      ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      shape : Deps.BlockedValueShape.t;
      ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]
  end
end

include T
