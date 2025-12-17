open Common

let init () =
  let code =
    native_fn "char.code" (fun _ty ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "char.code: %s" msg;
              return (V_Error |> Value.inferred ~span)
            in
            let c =
              arg |> Value.expect_char
              |> Option.unwrap_or_else (error "arg must be char")
            in
            V_Int32 (Char.code c |> Int32.of_int) |> Value.inferred ~span))
  in
  let from_code =
    native_fn "char.from_code" (fun _ty ~caller ~state:_ arg : value ->
        with_return (fun { return } ->
            let error msg () =
              Error.error caller "char.from_code: %s" msg;
              return (V_Error |> Value.inferred ~span)
            in
            let code =
              arg |> Value.expect_int32
              |> Option.unwrap_or_else
                   (error
                      (make_string "arg must be uint32, got %a" Value.print arg))
            in
            V_Char (Char.chr (Int32.to_int code)) |> Value.inferred ~span))
  in
  [ code; from_code ]
