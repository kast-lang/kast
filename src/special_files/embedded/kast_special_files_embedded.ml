open Std
open Kast_util

let std : Included_dir.included_dir = [%include_dir "std"]

let rec find_in : Included_dir.included_dir -> string -> string =
 fun dir path ->
  match String.index_opt path '/' with
  | Some idx -> (
      let dir_name = String.sub path 0 idx in
      let rest = String.sub path (idx + 1) (String.length path - (idx + 1)) in
      match dir.entries |> StringMap.find_opt dir_name with
      | Some (Dir dir) -> find_in dir rest
      | Some _ -> fail "%S is not a dir" dir_name
      | None -> fail "%S doesnt exist" dir_name)
  | None -> (
      match dir.entries |> StringMap.find_opt path with
      | Some (File { contents }) -> contents
      | Some _ -> fail "%S is not a file" path
      | None -> fail "%S doesnt exist" path)

let with_special_files : 'a. (unit -> 'a) -> 'a =
 fun (type a) (f : unit -> a) ->
  try f ()
  with effect Source.ReadSpecial s, k -> (
    match s |> String.strip_prefix ~prefix:"std/" with
    | None -> fail "Unknown special file %S" s
    | Some std_path ->
        let contents = find_in std std_path in
        Effect.Deep.continue k contents)
