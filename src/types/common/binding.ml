open Std
open Kast_util

module type Deps = sig
  module Ty : sig
    type t
  end
end

module type S = sig
  module Deps : Deps

  type t = {
    id : Id.t;
    name : Symbol.t;
    span : Span.t;
    ty : Deps.Ty.t;
    label : Label.t;
    mut : bool;
  }
  [@@deriving eq, ord]
end

module Make (Deps : Deps) : S = struct
  module Deps = Deps

  type t = {
    id : Id.t;
    name : Symbol.t;
    span : Span.t;
    ty : Deps.Ty.t;
    label : Label.t;
    mut : bool;
  }

  let equal a b = Id.equal a.id b.id
  let compare a b = Id.compare a.id b.id
end
