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
