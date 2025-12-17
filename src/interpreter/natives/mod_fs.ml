open Common

let init () =
  let read_file =
    native_fn "fs.read_file" (fun _ty ~caller ~state:_ arg : value ->
        match arg |> Value.await_inferred with
        | V_String path ->
            let contents = read_from_filesystem path in
            V_String contents |> Value.inferred ~span
        | _ ->
            Error.error caller "fs.read_file expected string arg";
            V_Error |> Value.inferred ~span)
  in
  [ read_file ]
