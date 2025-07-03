open Std
open Kast_util

let with_special_files f =
  try f ()
  with effect Source.ReadSpecial s, k -> (
    match s |> String.strip_prefix ~prefix:"std/" with
    | None -> fail "Unknown special file %S" s
    | Some std_path -> (
        match std_path with
        | "lib.ks" ->
            let contents = [%blob "../../../std/lib.ks"] in
            Effect.Deep.continue k contents
        | "io/_mod.ks" ->
            let contents = [%blob "../../../std/io/_mod.ks"] in
            Effect.Deep.continue k contents
        | _ -> fail "not in std: %S" std_path))
