open Std
open Kast_util
open Kast_types
include Kast_inference_base

let rec _unused = ()

and unite_ty_shape : Ty.Shape.t -> Ty.Shape.t -> Ty.Shape.t =
 fun a b ->
  let fail () =
    fail "can't unite %a and %a" Ty.Shape.print a Ty.Shape.print b
  in
  match (a, b) with
  | T_Unit, T_Unit -> T_Unit
  | T_Unit, _ -> fail ()
  | T_Int32, T_Int32 -> T_Int32
  | T_Int32, _ -> fail ()
  | T_String, T_String -> T_String
  | T_String, _ -> fail ()
  | T_Tuple { tuple = a }, T_Tuple { tuple = b } ->
      T_Tuple
        {
          tuple =
            Tuple.zip_order_a a b |> Tuple.map (fun (a, b) -> unite_ty a b);
        }
  | T_Tuple _, _ -> fail ()
  | T_Ty, T_Ty -> T_Ty
  | T_Ty, _ -> fail ()
  | T_Fn a, T_Fn b ->
      T_Fn { arg = unite_ty a.arg b.arg; result = unite_ty a.result b.result }
  | T_Fn _, _ -> fail ()

and unite_ty : ty -> ty -> ty =
 fun { var = a } { var = b } -> { var = Var.unite unite_ty_shape a b }

let expect unite expected x = ignore <| unite expected x

module Ty = struct
  let expect_inferred_as expected ty = expect unite_ty expected ty
end
