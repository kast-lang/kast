open Std
open Kast_util
module Parser = Kast_parser
module Compiler = Kast_compiler
open Kast_types

type global_state = {
  workspaces : workspace_state list;
  mutable vfs : string UriMap.t;
  imported : Compiler.imported;
  mutable diagnostics : Lsp.Types.Diagnostic.t list UriMap.t;
  root_of_included : (Uri.t, Uri.t) Hashtbl.t;
}

and workspace_state = {
  root : Uri.t;
  mutable files : file_state UriMap.t;
}

and file_state = {
  uri : Uri.t;
  parsed : Parser.result option;
  compiled : expr option;
}

let process_file (global : global_state) (source : source) : file_state =
  Log.trace (fun log -> log "PROJECT: processing %a" Uri.print source.uri);
  Hashtbl.remove global.root_of_included source.uri;

  let add_diagnostic uri (diag : Lsp.Types.Diagnostic.t) : unit =
    Log.trace (fun log -> log "Added diag for %a" Uri.print uri);
    global.diagnostics <-
      UriMap.update uri
        (fun prev ->
          let current = prev |> Option.unwrap_or_else (fun () -> []) in
          Some (diag :: current))
        global.diagnostics
  in
  let parsed =
    try
      let result = Parser.parse source Kast_default_syntax.ruleset in
      Some result
    with
    | effect Parser.Error.Error error, k ->
        Log.error (fun log -> log "%a" Parser.Error.print error);
        add_diagnostic error.span.uri
          {
            range = error.span |> Common.span_to_range;
            severity = Some Error;
            code = None;
            codeDescription = None;
            source = None;
            message = `String (make_string "%t" error.msg);
            tags = None;
            relatedInformation = None;
            data = None;
          };
        Effect.continue k ()
    (* TODO msg about crash? *)
    | _ -> None
  in
  let ast = Option.bind parsed (fun ({ ast; _ } : Parser.result) -> ast) in
  let compiled =
    Option.bind ast (fun ast ->
        try
          let compiler = Compiler.default ~imported:global.imported () in
          Some (Compiler.compile compiler Expr ast)
        with
        | effect Kast_interpreter.Error.Error error, k ->
            Log.error (fun log -> log "%a" Kast_interpreter.Error.print error);
            add_diagnostic error.span.uri
              {
                range = error.span |> Common.span_to_range;
                severity = Some Error;
                code = None;
                codeDescription = None;
                source = None;
                message = `String (make_string "%t" error.msg);
                tags = None;
                relatedInformation = None;
                data = None;
              };
            Effect.continue k ()
        | effect Compiler.Error.Error error, k ->
            Log.error (fun log -> log "%a" Compiler.Error.print error);
            add_diagnostic error.span.uri
              {
                range = error.span |> Common.span_to_range;
                severity = Some Error;
                code = None;
                codeDescription = None;
                source = None;
                message = `String (make_string "%t" error.msg);
                tags = None;
                relatedInformation = None;
                data = None;
              };
            Effect.continue k ()
        | effect Kast_inference.Error.Error error, k ->
            Log.error (fun log -> log "%a" Kast_inference.Error.print error);
            add_diagnostic error.span.uri
              {
                range = error.span |> Common.span_to_range;
                severity = Some Error;
                code = None;
                codeDescription = None;
                source = None;
                message = `String (make_string "%t" error.msg);
                tags = None;
                relatedInformation = None;
                data = None;
              };
            Effect.continue k ()
        | _ -> None)
  in
  { uri = source.uri; parsed; compiled }

let workspace_file (root : Uri.t) (path : string) =
  Uri.with_path root (Uri.path root ^ "/" ^ path)

let process_workspace (global : global_state) (workspace : workspace_state) =
  (* workspace.files <- UriMap.empty; *)
  try
    let handle_processed uri file_state =
      Log.trace (fun log -> log "File processed %a" Uri.print uri);
      workspace.files <- UriMap.add uri file_state workspace.files
    in
    try
      let workspace_ks =
        Source.read (workspace_file workspace.root "workspace.ks")
      in
      let processed_workspace_ks = process_file global workspace_ks in
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
             let processed = process_file global (Source.read uri) in
             handle_processed uri processed)
    with
    | effect Kast_compiler.Effect.FileStartedProcessing uri, k ->
        Log.trace (fun log -> log "removed diags for %a" Uri.print uri);
        global.diagnostics <- UriMap.remove uri global.diagnostics;
        Effect.Deep.continue k ()
    | effect
        Compiler.Effect.FileIncluded { root; uri; parsed; kind; compiled }, k ->
        Hashtbl.add global.root_of_included uri root;
        (match kind with
        | Expr ->
            let file_state : file_state =
              { uri; parsed = Some parsed; compiled = Some compiled }
            in
            handle_processed uri file_state
        | _ -> ());
        Effect.Deep.continue k ()
    | effect
        Compiler.Effect.FileImported { uri; parsed; compiled; value = _ }, k ->
        Hashtbl.remove global.root_of_included uri;
        let file_state : file_state =
          { uri; parsed = Some parsed; compiled = Some compiled }
        in
        handle_processed uri file_state;
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

let init_workspace (global : global_state) (root : Uri.t) : workspace_state =
  Log.trace (fun log -> log "Initializing workspace at %a" Uri.print root);
  let workspace : workspace_state = { root; files = UriMap.empty } in
  process_workspace global workspace;
  workspace

let init (workspaces : Lsp.Uri.t list) : global_state =
  let bootstrap : global_state =
    {
      workspaces = [];
      vfs = UriMap.empty;
      imported = Compiler.init_imported ();
      diagnostics = UriMap.empty;
      root_of_included = Hashtbl.create 0;
    }
  in
  let workspaces =
    workspaces
    |> List.map Common.uri_from_lsp
    |> List.map (init_workspace bootstrap)
  in
  { bootstrap with workspaces }

let file_state (state : global_state) (uri : Lsp.Uri.t) : file_state option =
  let uri = Common.uri_from_lsp uri in
  Log.trace (fun log -> log "PROJECT: find file state %a" Uri.print uri);
  state.workspaces
  |> List.find_map (fun workspace -> UriMap.find_opt uri workspace.files)

let update_file (global : global_state) (uri : Lsp.Uri.t) (source : string) :
    unit =
  let uri = Common.uri_from_lsp uri in
  Log.trace (fun log -> log "PROJECT: update %a" Uri.print uri);
  global.imported.by_uri <-
    UriMap.remove
      (Hashtbl.find_opt global.root_of_included uri |> Option.value ~default:uri)
      global.imported.by_uri;
  global.vfs <- UriMap.add uri source global.vfs

let recalculate (global : global_state) : unit =
  timed "recalculating lsp stuff" (fun () ->
      try global.workspaces |> List.iter (process_workspace global)
      with effect (Source.Read uri as eff), k -> (
        match UriMap.find_opt uri global.vfs with
        | Some contents -> Effect.continue k contents
        | None -> Effect.continue k (Effect.perform eff)))
