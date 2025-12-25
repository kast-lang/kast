open Kast_common

module Shape = struct
  module type Deps = sig
    module VarScope : Inference.Scope

    module Value : sig
      type t [@@deriving eq, ord]

      val scope : t -> VarScope.t
      val unite : t Inference.unite
    end
  end

  module type S = sig
    module Deps : Deps

    type part =
      | Uri of Uri.t
      | Str of string
      | Symbol of Symbol.t
    [@@deriving eq, ord]

    type instantiation = {
      generic : Deps.Value.t;
      arg : Deps.Value.t;
    }
    [@@deriving eq, ord]

    type t =
      | Simple of part
      | Concat of t * part
      | Instantiation of instantiation
    [@@deriving eq, ord]

    module Scope : Inference.Scope

    val scope : t -> Scope.t
    val unite : t Inference.unite
    val error : unit -> t
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type part =
      | Uri of Uri.t
      | Str of string
      | Symbol of Symbol.t
    [@@deriving eq, ord]

    type instantiation = {
      generic : Deps.Value.t;
      arg : Deps.Value.t;
    }
    [@@deriving eq, ord]

    type t =
      | Simple of part
      | Concat of t * part
      | Instantiation of instantiation
    [@@deriving eq, ord]

    module Scope = Deps.VarScope

    let part_scope = function
      | Uri _ | Str _ | Symbol _ -> Scope.root ()

    let rec scope = function
      | Simple part -> part_scope part
      | Concat (a, b) -> Scope.common (scope a) (part_scope b)
      | Instantiation { generic; arg } ->
          Scope.common (Deps.Value.scope generic) (Deps.Value.scope arg)

    let unite ~span a b = failwith __LOC__
    let error () = failwith __LOC__
  end
end

module Optional = struct
  module type Deps = sig
    module OptionalNameVar : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type t = { var : Deps.OptionalNameVar.t } [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = { var : Deps.OptionalNameVar.t } [@@deriving eq, ord]
  end
end

module T = struct
  module type Deps = sig
    module NameVar : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type t = { var : Deps.NameVar.t } [@@deriving eq, ord]
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = { var : Deps.NameVar.t } [@@deriving eq, ord]
  end
end

include T
