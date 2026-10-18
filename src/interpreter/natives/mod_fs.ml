open Common

let init () =
  let read_file =
    native_fn "fs.read_file" (fun _ty ~caller ~state arg : value ->
      let arg = single_arg ~span ~state arg in
      match arg |> Value.expect_any_string with
      | Some path ->
        let contents = read_from_filesystem path in
        V_String contents |> Value.inferred ~span
      | None ->
        Error.error caller "fs.read_file expected string arg";
        V_Error |> Value.inferred ~span)
  in
  let read_file_bytes =
    native_fn "fs.read_file_bytes" (fun ty ~caller ~state arg : value ->
      let arg = single_arg ~span ~state arg in
      match arg |> Value.expect_any_string with
      | Some path ->
        let bytes =
          read_from_filesystem path
          |> Bytes.of_string
          |> Bytes.to_seq
          |> Seq.map (fun b ->
            V_Byte b |> Value.inferred ~span |> Place.init ~mut:Mutable)
          |> Dynarray.of_seq
        in
        (match ty.result |> Ty.await_inferred with
         | T_List element_ty ->
           V_List { ty = element_ty; elements = bytes } |> Value.inferred ~span
         | _ ->
           Error.error caller "fs.read_file_bytes returns a list";
           V_Error |> Value.inferred ~span)
      | None ->
        Error.error caller "fs.read_file_bytes expected string arg";
        V_Error |> Value.inferred ~span)
  in
  [ read_file; read_file_bytes ]
;;
