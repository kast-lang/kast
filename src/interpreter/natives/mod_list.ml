open Common

let init () =
  [ native_fn "List.new" (fun ty ~caller ~state:_ _args : value ->
      with_return (fun { return } ->
        let ty =
          match ty.result |> Ty.await_inferred with
          | T_List ty -> ty
          | _ ->
            Error.error caller "List.new returns a list";
            return (V_Error |> Value.inferred ~span)
        in
        Value.inferred ~span <| V_List { ty; elements = Dynarray.create () }))
  ; native_fn "List.length" (fun _ty ~caller ~state:_ args : value ->
      let arg = single_arg ~span args in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "List.length: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let ref =
          arg |> Value.expect_ref |> Option.unwrap_or_else (error "expected ref as arg")
        in
        let list =
          read_place ~span ref.place
          |> Value.expect_list
          |> Option.unwrap_or_else (error "expected ref to a list ")
        in
        V_Int32 (Int32.of_int (list.elements |> Dynarray.length)) |> Value.inferred ~span))
  ; native_fn "List.at" (fun _ty ~caller ~state:_ args : value ->
      let args = args |> Value.expect_tuple |> Option.get in
      let list, idx = args.tuple |> Tuple.unwrap_unnamed2 in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "List.at: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let list =
          list.place
          |> read_place ~span
          |> Value.expect_ref
          |> Option.unwrap_or_else (error "expected ref as arg")
        in
        let mut = list.mut in
        let list =
          read_place ~span list.place
          |> Value.expect_list
          |> Option.unwrap_or_else (error "expected ref to a list ")
        in
        let idx =
          idx.place
          |> read_place ~span
          |> Value.expect_int32
          |> Option.unwrap_or_else (error "expected idx to be int32")
        in
        V_Ref { mut; place = Dynarray.get list.elements (Int32.to_int idx) }
        |> Value.inferred ~span))
  ; native_fn "List.push_back" (fun _ty ~caller ~state:_ args : value ->
      let args = args |> Value.expect_tuple |> Option.get in
      let list, value = args.tuple |> Tuple.unwrap_unnamed2 in
      with_return (fun { return } ->
        let error msg () =
          Error.error caller "List.push_back: %s" msg;
          return (V_Error |> Value.inferred ~span)
        in
        let list =
          list.place
          |> read_place ~span
          |> Value.expect_ref
          |> Option.unwrap_or_else (error "expected ref as arg")
        in
        let list =
          read_place ~span list.place
          |> Value.expect_list
          |> Option.unwrap_or_else (error "expected ref to a list ")
        in
        let value = value.place |> claim ~span in
        Dynarray.add_last list.elements (Place.init ~mut:Inherit value);
        V_Unit |> Value.inferred ~span))
  ]
;;
