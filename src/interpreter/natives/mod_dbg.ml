open Common

let init () =
  [
    (* TODO dbg should be polymorphic *)
    native_fn "dbg.print" (fun _ty ~caller:_ ~state:_ arg : value ->
        println "%a" Value.print arg;
        V_Unit |> Value.inferred ~span);
  ]
