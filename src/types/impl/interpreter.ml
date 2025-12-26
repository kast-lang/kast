open Kast_common

module Scope = struct
  module type Deps = sig
    module Place : sig
      type t
    end

    module TyShape : sig
      type tuple_field
    end
  end

  module type S = sig
    module Deps : Deps

    type local = {
      place : Deps.Place.t;
      ty_field : Deps.TyShape.tuple_field;
    }

    and t = {
      id : Id.t;
      span : Span.t;
      mutable locals : local SymbolMap.t;
      parent : t option;
      recursive : bool;
      mutable closed : bool;
      mutable on_update : (symbol * (unit -> unit)) list;
    }

    module VarScope : Inference.Scope
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    module T = struct
      type local = {
        place : Deps.Place.t;
        ty_field : Deps.TyShape.tuple_field;
      }

      and t = {
        id : Id.t;
        span : Span.t;
        mutable locals : local SymbolMap.t;
        parent : t option;
        recursive : bool;
        mutable closed : bool;
        mutable on_update : (symbol * (unit -> unit)) list;
      }
    end

    include T

    module VarScope : Inference.Scope = struct
      type t = T.t option

      let root () : t = None

      let common (a : t) (b : t) : t =
        match (a, b) with
        | None, None | None, Some _ | Some _, None -> None
        | Some a, Some b -> failwith __LOC__
    end
  end
end

module T = struct
  module type Deps = sig
    module Ty : sig
      type t
    end

    module Value : sig
      type t
    end

    module ValueMap : sig
      type 'a t
    end

    module NameShape : sig
      type t
    end

    module InterpreterScope : sig
      type t
    end
  end

  module type S = sig
    module Deps : Deps

    type natives = { by_name : (Deps.Ty.t -> Deps.Value.t) StringMap.t }

    and instantiated_generics = {
      mutable map : Deps.Value.t Deps.ValueMap.t Id.Map.t;
    }

    and cast_impls = {
      mutable map : Deps.Value.t Deps.ValueMap.t Deps.ValueMap.t;
      mutable as_module : Deps.Value.t Deps.ValueMap.t;
    }

    and state = {
      natives : natives;
      scope : Deps.InterpreterScope.t;
      current_fn_natives : (id, Deps.Value.t) Hashtbl.t;
      mutable contexts : Deps.Value.t Id.Map.t;
      instantiated_generics : instantiated_generics;
      cast_impls : cast_impls;
      current_name : Deps.NameShape.t;
    }
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type natives = { by_name : (Deps.Ty.t -> Deps.Value.t) StringMap.t }

    and instantiated_generics = {
      mutable map : Deps.Value.t Deps.ValueMap.t Id.Map.t;
    }

    and cast_impls = {
      mutable map : Deps.Value.t Deps.ValueMap.t Deps.ValueMap.t;
      mutable as_module : Deps.Value.t Deps.ValueMap.t;
    }

    and state = {
      natives : natives;
      scope : Deps.InterpreterScope.t;
      current_fn_natives : (id, Deps.Value.t) Hashtbl.t;
      mutable contexts : Deps.Value.t Id.Map.t;
      instantiated_generics : instantiated_generics;
      cast_impls : cast_impls;
      current_name : Deps.NameShape.t;
    }
  end
end

include T
