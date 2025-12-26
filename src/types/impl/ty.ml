open Kast_common

module Shape = struct
  module type Deps = sig
    module Ty : sig
      type t [@@deriving eq, ord]
    end

    module VarScope : Inference.Scope

    module OptionalName : sig
      type t [@@deriving eq, ord]
    end

    module Name : sig
      type t [@@deriving eq, ord]
    end

    module Pattern : sig
      type t [@@deriving eq, ord]
    end

    module TyVariantRow : sig
      type t [@@deriving eq, ord]
    end

    module BlockedValue : sig
      type t [@@deriving eq, ord]
    end

    module Unsorted : sig
      type is_mutable [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type tuple_field = {
      ty : Deps.Ty.t;
      label : Label.t option;
    }
    [@@deriving eq, ord]

    type tuple = {
      name : Deps.OptionalName.t;
      tuple : tuple_field Tuple.t;
    }
    [@@deriving eq, ord]

    type fn = {
      arg : Deps.Ty.t;
      result : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type generic = {
      arg : Deps.Pattern.t;
      result : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type unwind_token = { result : Deps.Ty.t } [@@deriving eq, ord]
    type variant_data = { data : Deps.Ty.t option } [@@deriving eq, ord]

    type variant = {
      name : Deps.OptionalName.t;
      variants : Deps.TyVariantRow.t;
    }
    [@@deriving eq, ord]

    type opaque = { name : Deps.Name.t } [@@deriving eq, ord]

    type ref = {
      mut : Deps.Unsorted.is_mutable;
      referenced : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type t =
      | Unit
      | Bool
      | Int32
      | Int64
      | Float64
      | String
      | Char
      | Ref of ref
      | Variant of variant
      | Tuple of tuple
      | Ty
      | Fn of fn
      | Generic of generic
      | Ast
      | UnwindToken of unwind_token
      | Target
      | ContextTy
      | CompilerScope
      | Opaque of opaque
      | Blocked of Deps.BlockedValue.t
      | Error
    [@@deriving eq, ord]

    module Scope : Inference.Scope

    val error : unit -> t
    val scope : t -> Scope.t
    val unite : t Inference.unite
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type tuple_field = {
      ty : Deps.Ty.t;
      label : Label.t option;
    }
    [@@deriving eq, ord]

    type tuple = {
      name : Deps.OptionalName.t;
      tuple : tuple_field Tuple.t;
    }
    [@@deriving eq, ord]

    type fn = {
      arg : Deps.Ty.t;
      result : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type generic = {
      arg : Deps.Pattern.t;
      result : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type unwind_token = { result : Deps.Ty.t } [@@deriving eq, ord]
    type variant_data = { data : Deps.Ty.t option } [@@deriving eq, ord]

    type variant = {
      name : Deps.OptionalName.t;
      variants : Deps.TyVariantRow.t;
    }
    [@@deriving eq, ord]

    type opaque = { name : Deps.Name.t } [@@deriving eq, ord]

    type ref = {
      mut : Deps.Unsorted.is_mutable;
      referenced : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type t =
      | Unit
      | Bool
      | Int32
      | Int64
      | Float64
      | String
      | Char
      | Ref of ref
      | Variant of variant
      | Tuple of tuple
      | Ty
      | Fn of fn
      | Generic of generic
      | Ast
      | UnwindToken of unwind_token
      | Target
      | ContextTy
      | CompilerScope
      | Opaque of opaque
      | Blocked of Deps.BlockedValue.t
      | Error
    [@@deriving eq, ord]

    module Scope = Deps.VarScope

    let scope _ = failwith __LOC__
    let unite ~span a b = failwith __LOC__
    let error () = Error
  end
end

(* and TyVariantData : sig end = struct end
and TyVariantRow : Row.S = Row.Make (TyVariantData)
and TyVar : Inference.Var.S = Inference.Var.Make (TyShape)
*)

module VariantRow = struct
  module Field = struct
    module type Deps = sig
      module Ty : Inference.Inferrable
    end

    module type S = Inference.Inferrable

    module Make (Deps : Deps) : S = struct
      module Deps = Deps
      module Scope = Deps.Ty.Scope

      type t = { data : Deps.Ty.t option } [@@deriving eq, ord]

      let scope { data } =
        match data with
        | Some data -> Deps.Ty.scope data
        | None -> Deps.Ty.Scope.root ()

      let error () = failwith __LOC__
      let unite ~span a b = failwith __LOC__
    end
  end

  module type Deps = sig
    module TyVariantRowField : Inference.Inferrable
  end

  module type S = Row.S

  module Make (Deps : Deps) : S = Row.Make (Deps.TyVariantRowField)
end

module Var = struct
  module type S = Inference.Var.S

  module Make (Shape : Shape.S) : S = Inference.Var.Make (Shape)
end

module T = struct
  module type Deps = sig
    module VarScope : sig
      type t
    end

    module TyVar : sig
      type t [@@deriving eq, ord]

      val scope : t -> VarScope.t
      val unite : t Inference.unite
    end
  end

  module type S = sig
    module Deps : Deps

    type t = { var : Deps.TyVar.t } [@@deriving eq, ord]

    val scope : t -> Deps.VarScope.t
    val unite : t Inference.unite
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = { var : Deps.TyVar.t } [@@deriving eq, ord]

    let scope { var } = Deps.TyVar.scope var
    let unite ~span a b = { var = Deps.TyVar.unite ~span a.var b.var }
  end
end

include T
