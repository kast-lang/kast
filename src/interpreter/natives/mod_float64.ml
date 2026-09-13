open Common

let init () =
  let to_bits =
    native_fn "float64.to_bits" (fun _ty ~caller ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "float64.to_bits: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let c =
          arg
          |> Value.expect_float64
          |> Option.unwrap_or_else (error "arg must be float64")
        in
        V_UInt64 (Int64.bits_of_float c) |> Value.inferred ~span))
  in
  [ to_bits ]
;;
