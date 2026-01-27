include Std
include Kast_util
include Kast_types
include Kast_interpreter_core
module Inference = Kast_inference

let span = Span.of_ocaml __POS__

let single_arg ~span (args : value) : value =
  let args = args |> Value.expect_tuple |> Option.unwrap in
  let arg = args.tuple |> Tuple.unwrap_single_unnamed in
  claim ~span arg.place
;;

let make_args ~span (args : value tuple) (ty : Types.ty_tuple) : value =
  V_Tuple
    { tuple =
        Tuple.zip_order_a args ty.tuple
        |> Tuple.map (fun (arg, ty_field) : Types.value_tuple_field ->
          { place = Place.init ~mut:Inherit arg; span; ty_field })
    ; ty
    }
  |> Value.inferred ~span
;;

let make_single_arg ~span (arg : value) (ty : Types.ty_tuple) : value =
  make_args ~span (Tuple.make [ arg ] []) ty
;;

let native_fn name impl : string * (ty -> value) =
  ( name
  , fun ty ->
      let scope = VarScope.of_ty ty in
      let fn_ty : Types.ty_fn =
        { args = { ty = Ty.new_not_inferred ~scope ~span }
        ; result = Ty.new_not_inferred ~scope ~span
        }
      in
      ty |> Inference.Ty.expect_inferred_as ~span (T_Fn fn_ty |> Ty.inferred ~span);
      V_NativeFn { id = Id.gen (); ty = fn_ty; name; impl = impl fn_ty }
      |> Value.inferred ~span )
;;
