open Std
open Kast_util

type t = { root : dir }
and dir = { entries : entry StringMap.t }
and file = { contents : string }

and entry =
  | File of file
  | Dir of dir

type vfs = t

module FromFilesystem = struct
  let rec read_dir path : dir =
    (* TODO *)
    let path = path |> Path.as_file |> Option.get in
    {
      entries =
        Sys.readdir path |> Array.to_list
        |> List.map (fun entry ->
               (entry, read_entry (Path.File (Filename.concat path entry))))
        |> StringMap.of_list;
    }

  and read_file path : file =
    let path = path |> Path.as_file |> Option.get in
    let ch = In_channel.open_text path in
    Fun.protect
      ~finally:(fun () -> In_channel.close ch)
      (fun () ->
        let contents = In_channel.input_all ch in
        { contents })

  and read_entry path : entry =
    let path_s = path |> Path.as_file |> Option.get in
    if Sys.is_directory path_s then Dir (read_dir path)
    else File (read_file path)

  let read : path -> vfs = fun path -> { root = read_dir path }
end

let ensure_subdir dir ~subdir =
  match StringMap.find_opt subdir dir.entries with
  | None ->
      {
        entries =
          StringMap.add subdir (Dir { entries = StringMap.empty }) dir.entries;
      }
  | Some (Dir _) -> dir
  | Some (File _) -> fail "expected subdir, got file"

let rec write_file_to_dir dir ~path contents =
  match String.index_opt path '/' with
  | None -> { entries = StringMap.add path (File { contents }) dir.entries }
  | Some idx -> (
      let subdir_name = String.sub path 0 idx in
      let path = String.sub path (idx + 1) (String.length path - (idx + 1)) in
      let dir = ensure_subdir dir ~subdir:subdir_name in
      match StringMap.find subdir_name dir.entries with
      | Dir subdir ->
          {
            entries =
              StringMap.add subdir_name
                (Dir (write_file_to_dir subdir ~path contents))
                dir.entries;
          }
      | _ -> failwith "must be subdir")

let write_file : vfs -> path:string -> string -> vfs =
 fun vfs ~path contents -> { root = write_file_to_dir vfs.root ~path contents }

let rec read_from_dir dir path =
  match String.index_opt path '/' with
  | None -> (
      match StringMap.find_opt path dir.entries with
      | None -> raise Not_found
      | Some (File { contents }) -> contents
      | Some (Dir _) -> failwith "expected file, got dir")
  | Some idx -> (
      let subdir_name = String.sub path 0 idx in
      let path = String.sub path (idx + 1) (String.length path - (idx + 1)) in
      match StringMap.find_opt subdir_name dir.entries with
      | Some (Dir subdir) -> read_from_dir subdir path
      | Some (File _) -> failwith "expected dir, got file"
      | None -> raise Not_found)

let read_file : vfs -> string -> string =
 fun vfs path -> read_from_dir vfs.root path
