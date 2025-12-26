module Impl = struct
  module A = struct
    module type Deps = sig
      module Scope : sig
        type t

        val root : unit -> t
      end

      module B : sig
        type t

        val scope : t -> Scope.t
      end
    end

    module type S = sig
      module Deps : Deps

      type t

      val scope : t -> Deps.Scope.t
    end

    module Make (Deps : Deps) : S = struct
      module Deps = Deps

      type t =
        | Empty
        | B of Deps.B.t

      let scope = function
        | Empty -> Deps.Scope.root ()
        | B b -> Deps.B.scope b
    end
  end

  module B = struct
    module type Deps = sig
      module Scope : sig
        type t

        val root : unit -> t
      end

      module A : sig
        type t

        val scope : t -> Scope.t
      end
    end

    module type S = sig
      module Deps : Deps

      type t =
        | Empty
        | A of Deps.A.t

      val scope : t -> Deps.Scope.t
    end

    module Make (Deps : Deps) : S = struct
      module Deps = Deps

      type t =
        | Empty
        | A of Deps.A.t

      let scope = function
        | Empty -> Deps.Scope.root ()
        | A a -> Deps.A.scope a
    end
  end

  module Scope = struct
    module type S = sig
      type t

      val root : unit -> t
    end

    module T : S = struct
      type t = unit

      let root () = ()
    end
  end
end

module rec All : sig
  module A : Impl.A.S with type Deps.Scope.t = Scope.t
  module B : Impl.B.S with type Deps.Scope.t = Scope.t
  module Scope : Impl.Scope.S
end = struct
  module A = A
  module B = B
  module Scope = Scope
end

and A : (Impl.A.S with type Deps.Scope.t = Scope.t) = Impl.A.Make (All)
and B : (Impl.B.S with type Deps.Scope.t = Scope.t) = Impl.B.Make (All)
and Scope : Impl.Scope.S = Impl.Scope.T
