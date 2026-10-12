open Std
open Kast_util

module Args = struct
  type args = string list
  type t = args

  let parse : string list -> args = fun args -> args
end

let run (args : Args.t) =
  let dump_compile_flags = ref false in
  let do_link = ref true in
  let cc_args =
    let rec go args =
      match args with
      | "--dump-compile-flags" :: rest ->
        dump_compile_flags := true;
        go rest
      | "--nolink" :: rest ->
        do_link := false;
        go rest
      | _ -> args
    in
    go args
  in
  let cc =
    match Sys.getenv_opt "CC" with
    | Some cc -> cc
    | None -> "cc"
  in
  let cc_args =
    match Sys.getenv_opt "CFLAGS" with
    | Some flags ->
      (flags
       |> String.split_on (fun c -> c = ' ' || c = '\n')
       |> List.filter (fun s -> s <> ""))
      @ cc_args
    | None -> cc_args
  in
  let runtime_args =
    match Sys.getenv_opt "KAST_PATH" with
    | None -> fail "set KAST_PATH"
    | Some kast_path ->
      let c_files = Dynarray.create () in
      let rec walk_dir dir_path =
        let dir = Unix.opendir dir_path in
        try
          while true do
            let entry = Unix.readdir dir in
            if entry = "." || entry = ".."
            then ()
            else (
              let path = Filename.concat dir_path entry in
              match (Unix.stat path).st_kind with
              | Unix.S_DIR -> walk_dir path
              | Unix.S_REG | Unix.S_LNK ->
                if Filename.extension path = ".c" then Dynarray.add_last c_files path
              | _ -> ())
          done
        with
        | End_of_file -> ()
      in
      let c_runtime_path = Filename.concat kast_path "c-runtime" in
      walk_dir c_runtime_path;
      let compile_flags = [ "-I"; Filename.concat c_runtime_path "include" ] in
      let link_flags = Dynarray.to_list c_files in
      if !do_link then compile_flags @ link_flags else compile_flags
  in
  let cc_args = runtime_args @ cc_args in
  if !dump_compile_flags
  then cc_args |> List.iter Stdlib.print_endline
  else (
    if not !quiet
    then
      Log.info (fun log ->
        log "Launching %s with args %a" cc (List.print String.print_maybe_escaped) cc_args);
    let cc_exitcode = Sys.command (Filename.quote_command cc cc_args) in
    if cc_exitcode <> 0 then fail "cc exited with exit code %d" cc_exitcode)
;;
