open Std
open Kast_util

module Shape = struct
  module T = struct
    type t = ..
  end

  include T
  include Print.Make (T)

  type T.t += Error : T.t
  type T.t += Ty
  type T.t += Unit

  let init () =
    register_print (fun ty ->
        match ty with
        | Error -> Some (fun fmt -> fprintf fmt "@{<red><error>@}")
        | Ty -> Some (fun fmt -> fprintf fmt "type")
        | Unit -> Some (fun fmt -> fprintf fmt "()")
        | _ -> None)
end

type t = { var : Shape.t Inference.Var.t }

let inferred shape = { var = Inference.Var.new_inferred shape }
let error () = inferred Shape.Error
let new_not_inferred () = { var = Inference.Var.new_not_inferred () }
let print fmt { var } = Inference.Var.print Shape.print fmt var

module InferenceImpl = struct
  type unite_fn = span -> Shape.t -> Shape.t -> Shape.t option

  let unite_fns : unite_fn list Atomic.t = Atomic.make []

  let register : unite_fn -> unit =
   fun f -> Atomic.set unite_fns (f :: Atomic.get unite_fns)

  let unite : Shape.t Inference.unite =
   fun ~span a b ->
    Atomic.get unite_fns
    |> List.find_map (fun f -> f span a b)
    |> Option.unwrap_or_else (fun () ->
           fail "Forgot to register type unification fns? %a, %a" Shape.print a
             Shape.print b)

  let fail span a b =
    Error.throw span "type inference failed: %a != %a" Shape.print a Shape.print
      b;
    Some Shape.Error

  let init () =
    register (fun span a b ->
        let fail () = fail span a b in
        match (a, b) with
        | Shape.Error, _ | _, Shape.Error -> Some Shape.Error
        | Shape.Ty, Shape.Ty -> Some Shape.Ty
        | Shape.Ty, _ | _, Shape.Ty -> fail ()
        | Shape.Unit, Shape.Unit -> Some Shape.Unit
        | Shape.Unit, _ | _, Shape.Unit -> fail ()
        | _ -> None)
end

let infer_same_as ~span expected_ty ty =
  ty.var
  |> Inference.Var.infer_same_as InferenceImpl.unite ~span expected_ty.var

module Inference = InferenceImpl

let init () =
  Shape.init ();
  Inference.init ()
