open Kast_common

module Scope = struct
  module type Deps = sig
    module Binding : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      id : Id.t;
      parent : t option;
      recursive : bool;
      mutable bindings : Deps.Binding.t StringMap.t;
      mutable closed : bool;
      mutable on_update : (unit -> unit) list;
    }
    [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      id : Id.t;
      parent : t option;
      recursive : bool;
      mutable bindings : Deps.Binding.t StringMap.t;
      mutable closed : bool;
      mutable on_update : (unit -> unit) list;
    }

    let equal a b = Id.equal a.id b.id
    let compare a b = Id.compare a.id b.id
  end
end
