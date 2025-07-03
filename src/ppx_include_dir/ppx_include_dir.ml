open Ppxlib

let location_errorf ~loc =
  Format.ksprintf (fun err ->
      raise (Ocaml_common.Location.Error (Ocaml_common.Location.error ~loc err)))

(* Same as [List.find_map] introduced in OCaml 4.10. *)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with
      | Some _ as result -> result
      | None -> find_map f l)

(* Return the list of paths we should try using, in order. *)
let get_candidate_paths ~loc path =
  let source_dir =
    loc.Ocaml_common.Location.loc_start.pos_fname |> Filename.dirname
  in
  if Filename.is_relative path then
    let absolute_path = Filename.concat source_dir path in
    (* Try the path relative to the source file first, then the one relative to the
       current working directory (typically, the build directory). *)
    [ absolute_path; path ]
  else
    (* The user passed an absolute path. Use as is. *)
    [ path ]

let read_file path =
  try
    let file = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr file)
      (fun () -> Some (really_input_string file (in_channel_length file)))
  with _ -> None

let get_blob ~loc path =
  match find_map read_file (get_candidate_paths ~loc path) with
  | Some blob -> blob
  | None ->
      location_errorf ~loc "[%%include_dir] could not find or load file %s" path

let rec get_entries ~loc path =
  try
    let entries = Sys.readdir path in
    let entries =
      entries |> Array.to_list
      |> List.map (fun entry ->
             let entry_path = Filename.concat path entry in
             let entry_expr : expression =
               match Sys.is_directory entry_path with
               | true ->
                   Ast_builder.Default.pexp_construct ~loc
                     { txt = Lident "Dir"; loc }
                     (Some (include_dir ~loc entry_path))
               | false ->
                   let contents =
                     Ast_builder.Default.estring ~loc (get_blob ~loc entry_path)
                   in
                   let file =
                     Ast_builder.Default.pexp_record ~loc
                       [ ({ txt = Lident "contents"; loc }, contents) ]
                       None
                   in
                   Ast_builder.Default.pexp_construct ~loc
                     { txt = Ldot (Lident "Included_dir", "File"); loc }
                     (Some file)
             in
             Ast_builder.Default.pexp_tuple ~loc
               [ Ast_builder.Default.estring ~loc entry; entry_expr ])
    in
    Ast_builder.Default.elist ~loc entries
  with _ ->
    location_errorf ~loc "[%%include_dir] could not find or load dir %s" path

and include_dir ~loc path =
  let entries = get_entries ~loc path in
  let string_map_of_list =
    Ast_builder.Default.pexp_ident ~loc
      { txt = Ldot (Lident "StringMap", "of_list"); loc }
  in
  let entries =
    Ast_builder.Default.pexp_apply ~loc string_map_of_list
      [ (Nolabel, entries) ]
  in
  let expr =
    Ast_builder.Default.pexp_record ~loc
      [ ({ txt = Lident "entries"; loc }, entries) ]
      None
  in
  let ty : core_type =
    Ast_builder.Default.ptyp_constr ~loc
      { txt = Ldot (Lident "Included_dir", "included_dir"); loc }
      []
  in
  Ast_builder.Default.pexp_constraint ~loc expr ty

let expand ~ctxt path =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  include_dir ~loc path

let extension =
  Extension.V3.declare "include_dir" Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension extension
let () = Ppxlib.Driver.register_transformation ~rules:[ rule ] "ppx_include_dir"
