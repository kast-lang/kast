open Std
open Kast_util
open Kast_types
include Kast_inference_base
include Error

let expect ~span unite expected x = ignore <| unite ~span expected x
let unite_ty = Inference_impl.unite_ty

module Ty = struct
  let expect_inferred_as ~span expected ty = expect ~span unite_ty expected ty
end

module Value = struct
  let expect_inferred_as ~span expected value =
    expect ~span Inference_impl.unite_value expected value
  ;;
end

let not_inferred_simple : 'a. span:span -> ('a, VarScope.t) var =
  fun ~span -> Var.new_not_inferred ~span ~scope:(VarScope.root ())
;;

let simple : 'a. span:span -> 'a -> ('a, VarScope.t) var =
  fun ~span value -> Var.new_inferred (fun _ -> VarScope.root ()) ~span value
;;

let await_inferred_simple : 'a. ('a, VarScope.t) var -> 'a =
  fun var -> Var.await_inferred ~error_shape:(fun _ -> failwith __LOC__) var
;;
