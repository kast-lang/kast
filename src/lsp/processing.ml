open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
open Kast_types

type global_state = {
  workspaces : workspace_state list;
  mutable vfs : string UriMap.t;
}

and workspace_state = {
  root : Uri.t;
  mutable files : file_state UriMap.t;
}

and file_state = {
  uri : Uri.t;
  parsed : Parser.result option;
  compiled : expr option;
  diagnostics : Lsp.Types.Diagnostic.t list;
}

let process_file (source : source) : file_state =
  Log.trace (fun log -> log "PROJECT: processing %a" Uri.print source.uri);
  let diagnostics = ref [] in
  let parsed =
    try
      let result = Parser.parse source Kast_default_syntax.ruleset in
      Some result
    with
    | effect Parser.Error.Error error, k ->
        Log.error (fun log -> log "%a" Parser.Error.print error);
        (* TODO might be from another file? *)
        diagnostics :=
          ({
             range = error.span |> Common.span_to_range;
             severity = Some Error;
             code = None;
             codeDescription = None;
             source = None;
             message = `String (make_string "%t" error.msg);
             tags = None;
             relatedInformation = None;
             data = None;
           }
            : Lsp.Types.Diagnostic.t)
          :: !diagnostics;
        Effect.continue k ()
    (* TODO msg about crash? *)
    | _ -> None
  in
  let ast = Option.bind parsed (fun ({ ast; _ } : Parser.result) -> ast) in
  let compiled =
    Option.bind ast (fun ast ->
        try
          let compiler = Compiler.default () in
          Some (Compiler.compile compiler Expr ast)
        with
        | effect Kast_interpreter.Error.Error error, k ->
            Log.error (fun log -> log "%a" Kast_interpreter.Error.print error);
            (* TODO might be from another file? *)
            diagnostics :=
              ({
                 range = error.span |> Common.span_to_range;
                 severity = Some Error;
                 code = None;
                 codeDescription = None;
                 source = None;
                 message = `String (make_string "%t" error.msg);
                 tags = None;
                 relatedInformation = None;
                 data = None;
               }
                : Lsp.Types.Diagnostic.t)
              :: !diagnostics;
            Effect.continue k ()
        | effect Compiler.Error.Error error, k ->
            Log.error (fun log -> log "%a" Compiler.Error.print error);
            (* TODO might be from another file? *)
            diagnostics :=
              ({
                 range = error.span |> Common.span_to_range;
                 severity = Some Error;
                 code = None;
                 codeDescription = None;
                 source = None;
                 message = `String (make_string "%t" error.msg);
                 tags = None;
                 relatedInformation = None;
                 data = None;
               }
                : Lsp.Types.Diagnostic.t)
              :: !diagnostics;
            Effect.continue k ()
        | effect Kast_inference.Error.Error error, k ->
            Log.error (fun log -> log "%a" Kast_inference.Error.print error);
            (* TODO might be from another file? *)
            diagnostics :=
              ({
                 range = error.span |> Common.span_to_range;
                 severity = Some Error;
                 code = None;
                 codeDescription = None;
                 source = None;
                 message = `String (make_string "%t" error.msg);
                 tags = None;
                 relatedInformation = None;
                 data = None;
               }
                : Lsp.Types.Diagnostic.t)
              :: !diagnostics;
            Effect.continue k ()
        | _ -> None)
  in
  { uri = source.uri; parsed; compiled; diagnostics = !diagnostics }

let workspace_file (root : Uri.t) (path : string) =
  Uri.with_path root (Uri.path root ^ "/" ^ path)

let process_workspace (workspace : workspace_state) =
  workspace.files <- UriMap.empty;
  try
    let handle_processed uri file_state =
      Log.trace (fun log -> log "File processed %a" Uri.print uri);
      workspace.files <- UriMap.add uri file_state workspace.files
    in
    try
      let workspace_ks =
        Source.read (workspace_file workspace.root "workspace.ks")
      in
      let processed_workspace_ks = process_file workspace_ks in
      handle_processed workspace_ks.uri processed_workspace_ks;
      let compiled = processed_workspace_ks.compiled |> Option.get in
      let evaled =
        Kast_interpreter.eval (Kast_interpreter.default ()) compiled
      in
      let workspace_roots = evaled |> Value.expect_tuple |> Option.get in
      let workspace_roots =
        workspace_roots.tuple.unnamed |> Array.to_list
        |> List.map (fun value -> value |> Value.expect_string |> Option.get)
      in
      Log.trace (fun log ->
          log "WORKSPACE ROOTS = %a"
            (List.print String.print_dbg)
            workspace_roots);
      workspace_roots
      |> List.iter (fun path ->
             let uri = workspace_file workspace.root path in
             let processed = process_file (Source.read uri) in
             handle_processed uri processed)
    with
    | effect Compiler.Effect.FileIncluded { uri; parsed; kind; compiled }, k ->
        (match kind with
        | Expr ->
            let file_state : file_state =
              {
                uri;
                parsed = Some parsed;
                compiled = Some compiled;
                diagnostics = [];
              }
            in
            handle_processed uri file_state
        | _ -> ());
        Effect.Deep.continue k ()
    | effect (Source.Read uri as eff), k ->
        if Uri.scheme uri = Some "workspace" then
          let uri = Uri.with_scheme uri None in
          let uri = Uri.append_if_relative workspace.root uri in
          let contents = Effect.perform (Source.Read uri) in
          Effect.Deep.continue k contents
        else Effect.Deep.continue k (Effect.perform eff)
  with exc ->
    Log.error (fun log ->
        log "Failed to process workspace: %s" (Printexc.to_string exc));
    ()

let init_workspace (root : Uri.t) : workspace_state =
  Log.trace (fun log -> log "Initializing workspace at %a" Uri.print root);
  let workspace : workspace_state = { root; files = UriMap.empty } in
  process_workspace workspace;
  workspace

let init (workspaces : Lsp.Uri.t list) : global_state =
  {
    workspaces =
      workspaces |> List.map Common.uri_from_lsp |> List.map init_workspace;
    vfs = UriMap.empty;
  }

let file_state (state : global_state) (uri : Lsp.Uri.t) : file_state option =
  let uri = Common.uri_from_lsp uri in
  Log.trace (fun log -> log "PROJECT: find file state %a" Uri.print uri);
  state.workspaces
  |> List.find_map (fun workspace -> UriMap.find_opt uri workspace.files)

let update_file (state : global_state) (uri : Lsp.Uri.t) (source : string) :
    unit =
  let uri = Common.uri_from_lsp uri in
  Log.trace (fun log -> log "PROJECT: update %a" Uri.print uri);
  state.vfs <- UriMap.add uri source state.vfs

let recalculate (state : global_state) : unit =
  timed "recalculating lsp stuff" (fun () ->
      try state.workspaces |> List.iter process_workspace
      with effect (Source.Read uri as eff), k -> (
        match UriMap.find_opt uri state.vfs with
        | Some contents -> Effect.continue k contents
        | None -> Effect.continue k (Effect.perform eff)))
