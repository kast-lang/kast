open Common

let init () =
  [ native_fn "string.at" (fun _ty ~caller ~state:_ args : value ->
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "string.at: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let args =
          args |> Value.expect_tuple |> Option.unwrap_or_else (error "arg must be tuple")
        in
        if not (args.tuple |> Tuple.is_unnamed 2)
        then error "expected 2 unnamed fields" ();
        let s, idx = args.tuple |> Tuple.unwrap_unnamed2 in
        let s =
          s.place
          |> claim ~span:caller
          |> Value.expect_string
          |> Option.unwrap_or_else (error "expected string as first arg")
        in
        let idx =
          idx.place
          |> claim ~span:caller
          |> Value.expect_int32
          |> Option.unwrap_or_else (error "expected idx be int32")
        in
        V_Char (String.get s (Int32.to_int idx) |> Option.unwrap_or_else (error "oob"))
        |> Value.inferred ~span))
  ; native_fn "string.length" (fun _ty ~caller ~state:_ args : value ->
      let arg = single_arg ~span args in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "string.length: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let s =
          arg
          |> Value.expect_string
          |> Option.unwrap_or_else (error "expected string as arg")
        in
        V_Int32 (Int32.of_int (String.length s)) |> Value.inferred ~span))
  ; native_fn "string.substring" (fun _ty ~caller ~state:_ args : value ->
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "string.substring: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let args =
          args |> Value.expect_tuple |> Option.unwrap_or_else (error "arg must be tuple")
        in
        if not (args.tuple |> Tuple.is_unnamed 3)
        then error "expected 2 unnamed fields" ();
        let s, start, len = args.tuple |> Tuple.unwrap_unnamed3 in
        let s =
          s.place
          |> claim ~span:caller
          |> Value.expect_string
          |> Option.unwrap_or_else (error "expected string as first arg")
        in
        let start =
          start.place
          |> claim ~span:caller
          |> Value.expect_int32
          |> Option.unwrap_or_else (error "expected start be int32")
        in
        let len =
          len.place
          |> claim ~span:caller
          |> Value.expect_int32
          |> Option.unwrap_or_else (error "expected len be int32")
        in
        V_String (String.sub s (Int32.to_int start) (Int32.to_int len))
        |> Value.inferred ~span))
  ; native_fn "string.iter" (fun _ty ~caller ~state args : value ->
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "string.iter: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let args =
          args |> Value.expect_tuple |> Option.unwrap_or_else (error "arg must be tuple")
        in
        if not (args.tuple |> Tuple.is_unnamed 2)
        then error "expected 2 unnamed fields" ();
        let s, f = args.tuple |> Tuple.unwrap_unnamed2 in
        let s =
          s.place
          |> claim ~span:caller
          |> Value.expect_string
          |> Option.unwrap_or_else (error "expected string as first arg")
        in
        let f = f.place |> claim ~span:caller in
        let _ =
          f
          |> Value.expect_fn
          |> Option.unwrap_or_else (error "expected fn as second arg")
        in
        s
        |> String.iter (fun c ->
          let c : value = V_Char c |> Value.inferred ~span in
          ignore <| call caller state f c;
          ());
        V_Unit |> Value.inferred ~span))
  ; native_fn "to_string" (fun _ty ~caller ~state:_ args ->
      let arg = single_arg ~span args in
      with_return (fun { return } : Value.Shape.t ->
        let s =
          match arg |> Value.await_inferred with
          | V_Bool value -> Bool.to_string value
          | V_Char value -> String.make 1 value
          | V_Int32 value -> Int32.to_string value
          | V_Int64 value -> Int64.to_string value
          | V_Float64 value -> Float.to_string value
          | shape ->
            Error.error caller "to_string doesn't work for %a" Value.Shape.print shape;
            return (V_Error : Value.Shape.t)
        in
        V_String s)
      |> Value.inferred ~span)
  ; native_fn "parse" (fun ty ~caller ~state:_ args ->
      let { arg = _; result = result_ty } : Types.ty_fn = ty in
      let arg = single_arg ~span args in
      match arg |> Value.await_inferred with
      | V_String s ->
        let shape : Value.shape =
          let parsed =
            match result_ty |> Ty.await_inferred with
            | T_Int32 ->
              Int32.of_string_opt s |> Option.map (fun x : Value.shape -> V_Int32 x)
            | T_Int64 ->
              Int64.of_string_opt s |> Option.map (fun x : Value.shape -> V_Int64 x)
            | T_Float64 ->
              Float.of_string_opt s |> Option.map (fun x : Value.shape -> V_Float64 x)
            | _ ->
              Error.error caller "no idea how to parse %a" Ty.print result_ty;
              Some V_Error
          in
          match parsed with
          | Some value -> value
          | None ->
            Error.error caller "could not parse %S as %a" s Ty.print result_ty;
            V_Error
        in
        shape |> Value.inferred ~span
      | _ ->
        Error.error caller "string_to_int32 expected a string";
        V_Error |> Value.inferred ~span)
  ]
;;
