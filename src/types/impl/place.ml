open Kast_common

module type Deps = sig
  module Value : sig
    type t [@@deriving eq, ord]
  end

  module Ty : sig
    type t [@@deriving eq, ord]
  end
end

module type S = sig
  module Deps : Deps

  type mut =
    | Immutable
    | Mutable
    | Inherit
  [@@deriving eq, ord]

  type state =
    | Uninitialized
    | Occupied of Deps.Value.t
    | MovedOut
  [@@deriving eq, ord]

  type t = {
    id : Id.t;
    mutable state : state;
    ty : Deps.Ty.t;
    mut : mut;
  }
  [@@deriving eq, ord]
end

module Make (Deps : Deps) : S = struct
  module Deps = Deps

  type mut =
    | Immutable
    | Mutable
    | Inherit
  [@@deriving eq, ord]

  type state =
    | Uninitialized
    | Occupied of Deps.Value.t
    | MovedOut
  [@@deriving eq, ord]

  type t = {
    id : Id.t;
    mutable state : state;
    ty : Deps.Ty.t;
    mut : mut;
  }

  let equal a b = Id.equal a.id b.id
  let compare a b = Id.compare a.id b.id
end
