open Common

let init () =
  let bin_op
        name
        (op_int32 : int32 -> int32 -> int32)
        (op_int64 : int64 -> int64 -> int64)
        (op_float : (float -> float -> float) option)
    =
    native_fn name (fun _ty ~caller ~state:_ args ->
      Kast_profiling.record
        (fun () -> make_string "native %S" name)
        (fun () ->
           match args |> Value.await_inferred with
           | V_Tuple { ty = _; tuple } ->
             let a, b = tuple |> Tuple.unwrap_unnamed2 in
             let a = a.place |> claim ~span:caller in
             let b = b.place |> claim ~span:caller in
             Log.trace (fun log ->
               log
                 "evaluating native bin_op %s: a=%a, b=%a"
                 name
                 Value.print
                 a
                 Value.print
                 b);
             let result : Value.shape =
               match a |> await_fully_inferred, b |> await_fully_inferred with
               | V_Int32 a, V_Int32 b -> V_Int32 (op_int32 a b)
               | V_Int64 a, V_Int64 b -> V_Int64 (op_int64 a b)
               | V_Float64 a, V_Float64 b ->
                 (match op_float with
                  | Some op_float -> V_Float64 (op_float a b)
                  | None -> V_Error)
               | V_String a, V_String b -> V_String (a ^ b) (* TODO only for + *)
               | _ -> V_Error
             in
             result |> Value.inferred ~span
           | _ ->
             Error.error caller "bin op %S expected a tuple as arg" name;
             V_Error |> Value.inferred ~span))
  in
  [ bin_op "+" Int32.add Int64.add (Some Float.add)
  ; bin_op "-" Int32.sub Int64.sub (Some Float.sub)
  ; bin_op "*" Int32.mul Int64.mul (Some Float.mul)
  ; bin_op "/" Int32.div Int64.div (Some Float.div)
  ; bin_op "%" Int32.rem Int64.rem (Some Float.rem)
  ; bin_op "bit_and" Int32.logand Int64.logand None
  ; bin_op "bit_or" Int32.logor Int64.logor None
  ; bin_op "bit_xor" Int32.logxor Int64.logxor None
  ; bin_op
      "bit_shift_left"
      (fun a b -> Int32.shift_left a (Int32.to_int b))
      (fun a b -> Int64.shift_left a (Int64.to_int b))
      None
  ; bin_op
      "bit_shift_right"
      (fun a b -> Int32.shift_right a (Int32.to_int b))
      (fun a b -> Int64.shift_right a (Int64.to_int b))
      None
  ; native_fn "bit_not" (fun _ty ~caller ~state:_ arg ->
      (match arg |> Value.await_inferred with
       | V_Int32 x -> V_Int32 (Int32.lognot x)
       | V_Int64 x -> V_Int64 (Int64.lognot x)
       | value ->
         Error.error caller "bit_not doesnt work with %a" Value.Shape.print value;
         V_Error)
      |> Value.inferred ~span)
  ; native_fn "unary -" (fun _ty ~caller ~state:_ args ->
      let arg = single_arg ~span args in
      (match arg |> Value.await_inferred with
       | V_Int32 x -> V_Int32 (Int32.neg x)
       | V_Int64 x -> V_Int64 (Int64.neg x)
       | V_Float64 x -> V_Float64 (Float.neg x)
       | value ->
         Error.error caller "unary - doesnt work with %a" Value.Shape.print value;
         V_Error)
      |> Value.inferred ~span)
  ]
;;
