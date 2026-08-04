open Common
module RecurseCache = Kast_inference_base.CompareRecurseCache

let init () =
  let cmp_fn name op =
    native_fn name (fun _ty ~caller ~state:_ value ->
      match value |> Value.await_inferred with
      | V_Tuple { ty = _; tuple } ->
        let a, b = tuple |> Tuple.unwrap_unnamed2 in
        let a = a.place |> claim ~span:caller |> await_fully_inferred in
        let b = b.place |> claim ~span:caller |> await_fully_inferred in
        let result : bool = op a b in
        V_Bool result |> Value.inferred ~span
      | _ ->
        Error.error caller "cmp op %a expected a tuple as arg" String.print_debug name;
        V_Error |> Value.inferred ~span)
  in
  let equal_value_shape a b =
    RecurseCache.with_cache (RecurseCache.create ()) (fun () ->
      Types.equal_value_shape a b)
  in
  [ cmp_fn "<" ( < )
  ; cmp_fn "<=" ( <= )
  ; cmp_fn "==" equal_value_shape
  ; cmp_fn "!=" (fun a b -> not (equal_value_shape a b))
  ; cmp_fn ">=" ( >= )
  ; cmp_fn ">" ( > )
  ; native_fn "cmp" (fun ty ~caller ~state:_ value ->
      match value |> Value.await_inferred with
      | V_Tuple { ty = _; tuple } ->
        let a, b = tuple |> Tuple.unwrap_unnamed2 in
        let a = a.place |> claim ~span:caller |> await_fully_inferred in
        let b = b.place |> claim ~span:caller |> await_fully_inferred in
        let cmp = compare a b in
        let variant =
          if cmp < 0 then "Less" else if cmp > 0 then "Greater" else "Equal"
        in
        Mod_reflection.construct_variant
          ~span:caller
          (ty.result |> Ty.await_inferred |> Ty.Shape.expect_variant |> Option.unwrap)
          variant
          None
      | _ ->
        Error.error caller "cmp op expected a tuple as arg";
        V_Error |> Value.inferred ~span)
  ]
;;
