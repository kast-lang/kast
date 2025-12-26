open Kast_common

module Shape = struct
  module type Deps = sig
    module Place : sig
      type t [@@deriving eq, ord]
    end

    module Value : sig
      type t [@@deriving eq, ord]
    end

    module TyShape : sig
      type opaque [@@deriving eq, ord]
      type generic [@@deriving eq, ord]
      type fn [@@deriving eq, ord]
      type tuple_field [@@deriving eq, ord]
      type tuple [@@deriving eq, ord]
      type variant [@@deriving eq, ord]
    end

    module Ty : sig
      type t [@@deriving eq, ord]
    end

    module CompilerScope : sig
      type t [@@deriving eq, ord]
    end

    module InterpreterScope : sig
      type t
    end

    module VarScope : Inference.Scope

    module Interpreter : sig
      type state
    end

    module NameShape : sig
      type t [@@deriving eq, ord]
    end

    module BlockedValue : sig
      type t [@@deriving eq, ord]
    end

    module Unsorted : sig
      type maybe_compiled_fn
    end
  end

  module type S = sig
    module Deps : Deps

    type ref = {
      mut : bool;
      place : Deps.Place.t;
    }
    [@@deriving eq, ord]

    type opaque = {
      ty : Deps.TyShape.opaque;
      value : Obj.t; [@equal Repr.equal] [@compare Stdlib.compare]
    }
    [@@deriving eq, ord]

    type target = { name : string } [@@deriving eq, ord]

    type context_ty = {
      id : Id.t;
      ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type unwind_token = {
      id : Id.t;
      result_ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    type untyped_fn = {
      id : Id.t;
      def : Deps.Unsorted.maybe_compiled_fn;
      calculated_natives : (id, Deps.Value.t) Hashtbl.t;
      captured : Deps.InterpreterScope.t;
    }
    [@@deriving eq, ord]

    type fn = {
      ty : Deps.TyShape.fn;
      fn : untyped_fn;
    }
    [@@deriving eq, ord]

    type generic = {
      id : Id.t;
      name : Deps.NameShape.t;
      fn : untyped_fn;
      ty : Deps.TyShape.generic;
    }
    [@@deriving eq, ord]

    type tuple_field = {
      place : Deps.Place.t;
      span : Span.t;
      ty_field : Deps.TyShape.tuple_field;
    }
    [@@deriving eq, ord]

    type tuple = {
      ty : Deps.TyShape.tuple;
      tuple : tuple_field Tuple.t;
    }
    [@@deriving eq, ord]

    type variant = {
      label : Label.t;
      data : Deps.Place.t option;
      ty : Deps.TyShape.variant;
    }
    [@@deriving eq, ord]

    type native_fn = {
      id : Id.t;
      name : string;
      ty : Deps.TyShape.fn;
      impl :
        caller:span ->
        state:Deps.Interpreter.state ->
        Deps.Value.t ->
        Deps.Value.t;
    }
    [@@deriving eq, ord]

    type t =
      | Unit
      | Bool of bool
      | Int32 of int32
      | Int64 of int64
      | Float64 of float
      | Char of char
      | Ref of ref
      | String of string
      | Tuple of tuple
      | Variant of variant
      | Ty of Deps.Ty.t
      | Fn of fn
      | Generic of generic
      | NativeFn of native_fn
      | Ast of Ast.t
      | UnwindToken of unwind_token
      | Target of target
      | ContextTy of context_ty
      | CompilerScope of Deps.CompilerScope.t
      | Opaque of opaque
      | Blocked of Deps.BlockedValue.t
      | Error
    [@@deriving eq, ord]

    module Scope = Deps.VarScope

    val error : unit -> t
    val scope : t -> Scope.t
    val unite : t Inference.unite
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type ref = {
      mut : bool;
      place : Deps.Place.t;
    }
    [@@deriving eq, ord]

    type opaque = {
      ty : Deps.TyShape.opaque;
      value : Obj.t; [@equal Repr.equal] [@compare Stdlib.compare]
    }
    [@@deriving eq, ord]

    type target = { name : string } [@@deriving eq, ord]

    type context_ty = {
      id : Id.t;
      ty : Deps.Ty.t;
    }

    let equal_context_ty a b = Id.equal a.id b.id
    let compare_context_ty a b = Id.compare a.id b.id

    type unwind_token = {
      id : Id.t;
      result_ty : Deps.Ty.t;
    }

    let equal_unwind_token a b = Id.equal a.id b.id
    let compare_unwind_token a b = Id.compare a.id b.id

    type untyped_fn = {
      id : Id.t;
      def : Deps.Unsorted.maybe_compiled_fn;
      calculated_natives : (id, Deps.Value.t) Hashtbl.t;
      captured : Deps.InterpreterScope.t;
    }

    let equal_untyped_fn a b = Id.equal a.id b.id
    let compare_untyped_fn a b = Id.compare a.id b.id

    type fn = {
      ty : Deps.TyShape.fn;
      fn : untyped_fn;
    }
    [@@deriving eq, ord]

    type generic = {
      id : Id.t;
      name : Deps.NameShape.t;
      fn : untyped_fn;
      ty : Deps.TyShape.generic;
    }
    [@@deriving eq, ord]

    type tuple_field = {
      place : Deps.Place.t;
      span : Span.t;
      ty_field : Deps.TyShape.tuple_field;
    }
    [@@deriving eq, ord]

    type tuple = {
      ty : Deps.TyShape.tuple;
      tuple : tuple_field Tuple.t;
    }
    [@@deriving eq, ord]

    type variant = {
      label : Label.t;
      data : Deps.Place.t option;
      ty : Deps.TyShape.variant;
    }
    [@@deriving eq, ord]

    type native_fn = {
      id : Id.t;
      name : string;
      ty : Deps.TyShape.fn;
      impl :
        caller:span ->
        state:Deps.Interpreter.state ->
        Deps.Value.t ->
        Deps.Value.t;
          [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
    }
    [@@deriving eq, ord]

    type t =
      | Unit
      | Bool of bool
      | Int32 of int32
      | Int64 of int64
      | Float64 of float
      | Char of char
      | Ref of ref
      | String of string
      | Tuple of tuple
      | Variant of variant
      | Ty of Deps.Ty.t
      | Fn of fn
      | Generic of generic
      | NativeFn of native_fn
      | Ast of Ast.t
      | UnwindToken of unwind_token
      | Target of target
      | ContextTy of context_ty
      | CompilerScope of Deps.CompilerScope.t
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

module Var = struct
  module type S = Inference.Var.S

  module Make (Shape : Shape.S) : S = Inference.Var.Make (Shape)
end

module T = struct
  module type Deps = sig
    module VarScope : sig
      type t

      val common : t -> t -> t
    end

    module ValueVar : sig
      type t [@@deriving eq, ord]

      val scope : t -> VarScope.t
      val unite : t Inference.unite
    end

    module Ty : sig
      type t [@@deriving eq, ord]

      val scope : t -> VarScope.t
      val unite : t Inference.unite
    end
  end

  module type S = sig
    module Deps : Deps

    type t = {
      var : Deps.ValueVar.t;
      ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    val scope : t -> Deps.VarScope.t
    val unite : t Inference.unite
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type t = {
      var : Deps.ValueVar.t;
      ty : Deps.Ty.t;
    }
    [@@deriving eq, ord]

    let scope { var; ty } =
      Deps.VarScope.common (Deps.ValueVar.scope var) (Deps.Ty.scope ty)

    let unite ~span a b =
      {
        var = Deps.ValueVar.unite ~span a.var b.var;
        ty = Deps.Ty.unite ~span a.ty b.ty;
      }
  end
end

include T

module Map = struct
  module type Deps = sig
    module Value : sig
      type t [@@deriving eq, ord]
    end
  end

  module type S = sig
    module Deps : Deps

    type 'a t
    type key = Deps.Value.t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find_opt : key -> 'a t -> 'a option
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  end

  module Make (Deps : Deps) : S = struct
    module Deps = Deps

    type key = Deps.Value.t
    type 'a t = { entries : (key * 'a) list }

    let empty = { entries = [] }
    let add key value map = { entries = (key, value) :: map.entries }

    let find_opt key map =
      map.entries
      |> List.find_map (fun (map_key, value) ->
          if Deps.Value.equal key map_key then Some value else None)

    let update (key : key) (f : 'a option -> 'a option) (map : 'a t) : 'a t =
      let existed = ref false in
      let updated_if_existed =
        map.entries
        |> List.filter_map (fun (map_key, current_value) ->
            (if Deps.Value.equal key map_key then (
               existed := true;
               f (Some current_value))
             else Some current_value)
            |> Option.map (fun new_value -> (map_key, new_value)))
      in
      if !existed then { entries = updated_if_existed }
      else
        match f None with
        | None -> map
        | Some value -> { entries = (key, value) :: map.entries }

    let iter f map = map.entries |> List.iter (fun (key, value) -> f key value)

    let union f a b =
      let result = ref a in
      b
      |> iter (fun key value ->
          result :=
            !result
            |> update key (fun current_value ->
                match current_value with
                | None -> Some value
                | Some current_value -> f key value current_value));
      !result
  end
end
