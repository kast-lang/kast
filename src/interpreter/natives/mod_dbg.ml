open Common

let init () =
  [ (* TODO dbg should be polymorphic *)
    native_fn "dbg.print" (fun _ty ~caller:_ ~state:_ args : value ->
      eprintln "%a" (Print.print_args ~open_:"" ~close:"") args;
      V_Unit |> Value.inferred ~span)
  ]
;;
