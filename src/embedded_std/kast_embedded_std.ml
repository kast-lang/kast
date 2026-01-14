open Std
open Kast_util

let std : Included_dir.included_dir = [%include_dir "%{project_root}/std"]

let rec find_in : Included_dir.included_dir -> string -> string =
  fun dir path ->
  match String.index_opt path '/' with
  | Some idx ->
    let dir_name = String.sub path 0 idx in
    let rest = String.sub path (idx + 1) (String.length path - (idx + 1)) in
    if idx = 0
    then find_in dir rest
    else (
      match dir.entries |> StringMap.find_opt dir_name with
      | Some (Dir dir) -> find_in dir rest
      | Some _ -> fail "%S is not a dir" dir_name
      | None -> fail "dir %S doesnt exist" dir_name)
  | None ->
    (match dir.entries |> StringMap.find_opt path with
     | Some (File { contents }) -> contents
     | Some _ -> fail "%S is not a file" path
     | None -> fail "file %S doesnt exist" path)
;;

let with_embedded_std : 'a. (unit -> 'a) -> 'a =
  fun (type a) (f : unit -> a) ->
  try f () with
  | effect Kast_compiler.Effect.FindStd, k ->
    Effect.Deep.continue k (Uri.of_string "std:")
  | effect Source.Read uri, k when Uri.scheme uri = Some "std" ->
    Effect.continue_with k (fun () -> find_in std (Uri.path uri))
;;
