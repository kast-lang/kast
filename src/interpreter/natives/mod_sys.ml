open Common

let argv : string array ref = ref [||]

let init () =
  let chdir =
    native_fn "sys.chdir" (fun _ty ~caller ~state:_ arg : value ->
      match arg |> Value.await_inferred with
      | V_String path ->
        Stdlib.Sys.chdir path;
        V_Unit |> Value.inferred ~span
      | _ ->
        Error.error caller "sys.chdir expected string arg";
        V_Error |> Value.inferred ~span)
  in
  let argc =
    native_fn "sys.argc" (fun _ty ~caller:_ ~state:_ _arg : value ->
      V_Int32 (!argv |> Array.length |> Int32.of_int) |> Value.inferred ~span)
  in
  let argv_at =
    native_fn "sys.argv_at" (fun _ty ~caller ~state:_ arg : value ->
      match arg |> Value.await_inferred with
      | V_Int32 idx ->
        (match Array.get_opt !argv (Int32.to_int idx) with
         | None ->
           Error.error caller "sys.argv_at out of bounds";
           V_Error |> Value.inferred ~span
         | Some arg -> V_String arg |> Value.inferred ~span)
      | _ ->
        Error.error caller "sys.argv_at expected int32 arg";
        V_Error |> Value.inferred ~span)
  in
  let exec =
    native_fn "sys.exec" (fun _ty ~caller ~state:_ arg : value ->
      match arg |> Value.await_inferred with
      | V_String cmd ->
        V_Int32 (Stdlib.Sys.command cmd |> Int32.of_int) |> Value.inferred ~span
      | _ ->
        Error.error caller "sys.exec expected string arg";
        V_Error |> Value.inferred ~span)
  in
  let get_env =
    native_fn "sys.get_env" (fun ty ~caller ~state:_ arg : value ->
      let sum_variant_ty =
        match Ty.await_inferred ty.result with
        | Types.T_Variant variant_ty -> variant_ty
        | _ -> unreachable "sys.get_env returns variant type `Option.t`"
      in
      (* Get label `:Some` of get_env's return type Option.t[string] *)
      let label_found, xs =
        match Inference.Var.inferred_opt sum_variant_ty.variants.var |> Option.get with
        | R_Cons { label; rest; _ } ->
          (* Assert label has name `Some` *)
          if Label.get_name label <> "Some"
          then unreachable "`Option.t` should have first variant Some";
          label, rest
        | _ -> unreachable "sys.get_env returns row of 2 variants"
      in
      (* Get label `:None` of get_env's return type Option.t[string] *)
      let label_not_found =
        match Inference.Var.inferred_opt xs.var |> Option.get with
        | R_Cons { label; rest; _ } ->
          (* Assert label has name `None` *)
          if Label.get_name label <> "None"
          then unreachable "`Option.t` should have second variant None";
          (* Assert this is the last variant *)
          (match Inference.Var.inferred_opt rest.var |> Option.get with
           | R_Empty -> ()
           | _ -> unreachable "sys.get_env returns row of 2 variants");
          label
        | _ -> unreachable "sys.get_env returns row of 2 variants"
      in
      match arg |> Value.await_inferred with
      | V_String var ->
        let env_val =
          try Some (Stdlib.Sys.getenv var) with
          | Not_found -> None
        in
        (match env_val with
         | Some s ->
           V_Variant
             { label = label_found
             ; data = Some (Place.init ~mut:Inherit (V_String s |> Value.inferred ~span))
             ; ty = sum_variant_ty
             }
           |> Value.inferred ~span
         | None ->
           V_Variant { label = label_not_found; data = None; ty = sum_variant_ty }
           |> Value.inferred ~span)
      | _ ->
        Error.error caller "sys.exec expected string arg";
        V_Error |> Value.inferred ~span)
  in
  let exit =
    native_fn "sys.exit" (fun _ty ~caller ~state:_ arg : value ->
      match arg |> Value.await_inferred with
      | V_Int32 idx -> Int32.to_int idx |> exit
      | _ ->
        Error.error caller "sys.exit expected int32 arg";
        V_Error |> Value.inferred ~span)
  in
  [ chdir; argc; argv_at; exec; get_env; exit ]
;;
