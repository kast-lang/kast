open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
open Kast_types

type global_state = { workspaces : workspace_state list }

and workspace_state = {
  root : Lsp.Uri.t;
  mutable processed : processed_project option;
  mutable files : file_state StringMap.t;
  mutable vfs : Vfs.t;
}

and processed_project = { roots : file_state list }

and file_state = {
  uri : Lsp.Uri.t;
  parser_error : Parser.error option;
  parsed : Parser.result option;
  compiler_error : Compiler.error option;
  type_error : Kast_inference.error option;
  compiled : expr option;
}

type read_file = {
  uri : Lsp.Uri.t;
  contents : string;
}

let process_file (file : read_file) : file_state =
  Log.info "PROJECT: processing %S" (Lsp.Uri.to_string file.uri);
  let source : source =
    { contents = file.contents; filename = File (Lsp.Uri.to_path file.uri) }
  in
  let parser_error, parsed =
    try
      let result = Parser.parse source Kast_default_syntax.ruleset in
      (None, Some result)
    with
    | Parser.Error error -> (Some error, None)
    (* TODO msg about crash? *)
    | _ -> (None, None)
  in
  let ast = Option.bind parsed (fun ({ ast; _ } : Parser.result) -> ast) in
  let compiler_error = ref None in
  let type_error = ref None in
  let compiled =
    Option.bind ast (fun ast ->
        let compiler = Compiler.default () in
        try Some (Compiler.compile compiler Expr ast) with
        | Compiler.Error error ->
            compiler_error := Some error;
            None
        | Kast_inference.Error error ->
            type_error := Some error;
            None
        | _ -> None)
  in
  {
    parser_error;
    uri = file.uri;
    parsed;
    compiler_error = !compiler_error;
    type_error = !type_error;
    compiled;
  }

let process_project ~(read_file : string -> read_file)
    ~(handle_processed : string -> file_state -> unit) : processed_project =
  try
    let project_ks = read_file "project.ks" in
    let processed_project_ks = process_file project_ks in
    handle_processed "project.ks" processed_project_ks;
    let compiled = processed_project_ks.compiled |> Option.get in
    let evaled = Kast_interpreter.eval (Kast_interpreter.default ()) compiled in
    let project_roots = evaled |> Value.expect_tuple in
    let project_roots =
      project_roots.tuple.unnamed |> Array.to_list
      |> List.map Value.expect_string
    in
    let roots =
      project_roots
      |> List.map (fun path ->
             let processed = process_file (read_file path) in
             handle_processed path processed;
             processed)
    in
    { roots }
  with effect Compiler.Effect.FileIncluded { path; ast; kind; compiled }, k ->
    Log.info "PROJECT: file included %a" Path.print path;
    Effect.Deep.continue k ()

let read_from_filesystem path =
  let ch = In_channel.open_text path in
  Fun.protect
    (fun () -> In_channel.input_all ch)
    ~finally:(fun () -> In_channel.close ch)

let init_workspace (root : Lsp.Uri.t) : workspace_state =
  Log.info "PROJECT: Initializing state at %S" (Lsp.Uri.to_string root);
  let root_path = Lsp.Uri.to_path root in
  let vfs = ref ({ root = { entries = StringMap.empty } } : Vfs.t) in
  let read_file =
   fun path ->
    let path = Filename.concat root_path path in
    let contents = read_from_filesystem path in
    vfs := Vfs.write_file !vfs ~path contents;
    { uri = Lsp.Uri.of_path path; contents }
  in
  let files = ref StringMap.empty in
  let handle_processed path processed =
    files := StringMap.add path processed !files
  in
  let processed =
    try Some (process_project ~read_file ~handle_processed) with _ -> None
  in
  { root; vfs = !vfs; processed; files = !files }

let init (workspaces : Lsp.Uri.t list) : global_state =
  { workspaces = workspaces |> List.map init_workspace }

let child_relative_path ~(parent : Lsp.Uri.t) ~(child : Lsp.Uri.t) :
    string option =
  let parent = parent |> Lsp.Uri.to_path in
  let child = child |> Lsp.Uri.to_path in
  child |> String.strip_prefix ~prefix:(parent ^ "/")

let file_state (state : global_state) (uri : Lsp.Uri.t) : file_state option =
  Log.info "PROJECT: find file state %S" (Lsp.Uri.to_string uri);
  match
    state.workspaces
    |> List.find_map (fun workspace ->
           let* path = child_relative_path ~parent:workspace.root ~child:uri in
           Some (workspace, path))
  with
  | Some (workspace, path) -> StringMap.find_opt path workspace.files
  | None -> None

let update_file (state : global_state) (uri : Lsp.Uri.t) (source : string) :
    unit =
  Log.info "PROJECT: update %S" (Lsp.Uri.to_string uri);
  match
    state.workspaces
    |> List.find_map (fun workspace ->
           let* path = child_relative_path ~parent:workspace.root ~child:uri in
           Some (workspace, path))
  with
  | Some (workspace, path) ->
      Log.info "PROJECT: Updating file %S as part of %S" path
        (Lsp.Uri.to_string workspace.root);
      workspace.vfs <- Vfs.write_file workspace.vfs ~path source;
      let read_file =
       fun path ->
        let contents =
          try Vfs.read_file workspace.vfs path
          with _ ->
            let path =
              Filename.concat (workspace.root |> Lsp.Uri.to_path) path
            in
            read_from_filesystem path
          (* | Failure s -> fail "Can't read %S: %s" path s
          | Not_found -> fail "%S not found" path *)
        in
        {
          uri =
            Lsp.Uri.of_path
              (Filename.concat (Lsp.Uri.to_path workspace.root) path);
          contents;
        }
      in
      workspace.files <- StringMap.empty;
      let handle_processed path processed =
        workspace.files <- StringMap.add path processed workspace.files
      in
      workspace.processed <-
        (try Some (process_project ~read_file ~handle_processed)
         with exc ->
           Log.error "PROJECT ERROR: %s" (Printexc.to_string exc);
           None)
  | None -> Log.info "PROJECT: %S is not part of state" (Lsp.Uri.to_string uri)
