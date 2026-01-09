open Common

type _ Effect.t += Input : string -> string Effect.t

let init () =
  [ native_fn "io.print" (fun _ty ~caller ~state:_ value ->
      (match value |> Value.await_inferred with
       | V_String s -> println "%s" s
       | _ -> Error.error caller "io.print expected a string");
      V_Unit |> Value.inferred ~span)
  ; native_fn "io.eprint" (fun _ty ~caller ~state:_ value ->
      (match value |> Value.await_inferred with
       | V_String s -> eprintln "%s" s
       | _ -> Error.error caller "io.eprint expected a string");
      V_Unit |> Value.inferred ~span)
  ; native_fn "io.input" (fun _ty ~caller ~state:_ value ->
      match value |> Value.await_inferred with
      | V_String s ->
        let line = Effect.perform (Input s) in
        V_String line |> Value.inferred ~span
      | _ ->
        Error.error caller "io.input expected a string";
        V_Error |> Value.inferred ~span)
  ]
;;
