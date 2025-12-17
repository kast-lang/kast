include Std
include Kast_util
include Kast_types
include Kast_interpreter_core
module Inference = Kast_inference

let span = Span.of_ocaml __POS__

let native_fn name impl : string * (ty -> value) =
  ( name,
    fun ty ->
      let fn_ty : Types.ty_fn =
        { arg = Ty.new_not_inferred ~span; result = Ty.new_not_inferred ~span }
      in
      ty
      |> Inference.Ty.expect_inferred_as ~span (T_Fn fn_ty |> Ty.inferred ~span);
      V_NativeFn { id = Id.gen (); ty = fn_ty; name; impl = impl fn_ty }
      |> Value.inferred ~span )
