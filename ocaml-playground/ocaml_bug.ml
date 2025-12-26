module Impl = struct
  module A = struct
    module type Deps = sig
      type scope
    end

    module type S = sig
      module Deps : Deps

      type t
    end

    module Make (Deps : Deps) : S with module Deps = Deps = struct
      module Deps = Deps

      type t = Empty
    end
  end

  module Scope = struct
    module type S = sig
      type t
    end

    module T : S = struct
      type t = unit
    end
  end
end

module rec All : sig
  type a = A.t
  type scope = Scope.t
end = struct
  type a = A.t
  type scope = Scope.t
end

and A : (Impl.A.S with type Deps.scope = Scope.t) = Impl.A.Make (All)
and Scope : Impl.Scope.S = Impl.Scope.T
