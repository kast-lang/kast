open Std
open Kast_util

let with_special_files f =
  try f ()
  with effect Source.ReadSpecial s, k -> (
    match s |> String.strip_prefix ~prefix:"std/" with
    | None -> fail "Unknown special file %S" s
    | Some std_path ->
        let path = Filename.concat "std" std_path in
        let contents = In_channel.input_all (In_channel.open_text path) in
        Effect.Deep.continue k contents)
