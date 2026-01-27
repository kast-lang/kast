open Common

let init () =
  let code =
    native_fn "char.code" (fun _ty ~caller ~state:_ arg : value ->
      let arg = single_arg ~span arg in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "char.code: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let c =
          arg |> Value.expect_char |> Option.unwrap_or_else (error "arg must be char")
        in
        V_Int32 (Uchar.to_int c |> Int32.of_int) |> Value.inferred ~span))
  in
  let from_code =
    native_fn "char.from_code" (fun _ty ~caller ~state:_ args : value ->
      let arg = single_arg ~span args in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "char.from_code: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let code =
          arg
          |> Value.expect_int32
          |> Option.unwrap_or_else
               (error (make_string "arg must be uint32, got %a" Value.print arg))
        in
        try
          let c = Uchar.of_int (Int32.to_int code) in
          V_Char c |> Value.inferred ~span
        with
        | Invalid_argument _ -> error "invalid utf8" ()))
  in
  [ code; from_code ]
;;
