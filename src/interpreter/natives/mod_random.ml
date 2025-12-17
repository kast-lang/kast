open Common

let init () =
  [
    native_fn "random.gen_range" (fun (ty : Types.ty_fn) ~caller ~state:_ arg ->
        (try
           let { ty = _; tuple } : Kast_types.Types.value_tuple =
             arg |> Value.expect_tuple |> Option.get
           in
           let min, max = tuple |> Tuple.unwrap_named2 [ "min"; "max" ] in
           let min = min.place |> claim ~span:caller in
           let max = max.place |> claim ~span:caller in
           match ty.result |> Ty.await_inferred with
           | T_Int32 ->
               let min = min |> Value.expect_int32 |> Option.get in
               let max = max |> Value.expect_int32 |> Option.get in
               V_Int32 (Stdlib.Random.int32_in_range ~min ~max)
           | T_Int64 ->
               let min = min |> Value.expect_int64 |> Option.get in
               let max = max |> Value.expect_int64 |> Option.get in
               V_Int64 (Stdlib.Random.int64_in_range ~min ~max)
           | T_Float64 ->
               let min = min |> Value.expect_float64 |> Option.get in
               let max = max |> Value.expect_float64 |> Option.get in
               V_Float64 (min +. Stdlib.Random.float (max -. min))
           | _ ->
               Error.error caller "idk how to generate %a" Ty.print ty.result;
               V_Error
         with
          | Cancel -> raise Cancel
          | exc ->
              Error.error caller "random.gen_range: %s" (Printexc.to_string exc);
              V_Error)
        |> Value.inferred ~span);
  ]
