open Std
open Kast_util
module Compiler = Kast_compiler
module Token = Kast_token
module Lexer = Kast_lexer
module Ast = Kast_ast
module Parser = Kast_parser
open Kast_types

module Args = struct
  type args = { dummy : unit }
  type t = args

  let parse : string list -> args = function
    | [] -> { dummy = () }
    | arg :: _rest -> fail "Unexpected arg %S" arg
end

class lsp_server =
  object (self)
    inherit Linol_eio.Jsonrpc2.server

    method! config_modify_capabilities (c : Lsp.Types.ServerCapabilities.t) :
        Lsp.Types.ServerCapabilities.t =
      {
        c with
        documentFormattingProvider =
          Some (`DocumentFormattingOptions Formatting.options);
        semanticTokensProvider =
          Some (`SemanticTokensRegistrationOptions Semantic_tokens.options);
        selectionRangeProvider =
          Some (`SelectionRangeRegistrationOptions Selection_range.options);
        inlayHintProvider =
          Some (`InlayHintRegistrationOptions Inlay_hints.options);
        hoverProvider = Some (`HoverOptions Hover.options);
        referencesProvider = Some (`ReferenceOptions Hover.references_options);
        definitionProvider = Some (`DefinitionOptions Hover.definition_options);
        renameProvider = Some (`RenameOptions Hover.rename_options);
      }

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, Processing.file_state) Hashtbl.t =
      Hashtbl.create 32

    method spawn_query_handler f = Linol_eio.spawn f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      Log.info "processing file %S" (Lsp.Uri.to_path uri);

      let new_state =
        Processing.process_file uri
          { filename = File (Lsp.Types.DocumentUri.to_path uri); contents }
      in
      Hashtbl.replace buffers uri new_state;
      let diags = Diagnostics.get new_state in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_eio.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_eio.t =
      Hashtbl.remove buffers d.uri;
      Linol_eio.return ()

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri
        ~(range : Lsp.Types.Range.t) () :
        Lsp.Types.InlayHint.t list option Linol_eio.t =
      Linol_eio.return (Hashtbl.find buffers uri |> Inlay_hints.get range)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        (_ : Linol_eio.doc_state) : Lsp.Types.Hover.t option Linol_eio.t =
      Linol_eio.return (Hashtbl.find buffers uri |> Hover.hover pos)

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ (_ : Linol_eio.doc_state) :
        Lsp.Types.Locations.t option Linol_eio.t =
      Linol_eio.return (Hashtbl.find buffers uri |> Hover.find_definition pos)

    method private on_req_selection_range :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.SelectionRangeParams.t ->
        Lsp.Types.SelectionRange.t list =
      fun ~notify_back:_ params ->
        Linol_eio.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Selection_range.get params)

    method private on_req_format :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.DocumentFormattingParams.t ->
        Lsp.Types.TextEdit.t list option =
      fun ~notify_back:_ params ->
        Linol_eio.return
          (Hashtbl.find buffers params.textDocument.uri |> Formatting.run params)

    method private on_semantic_tokens :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.SemanticTokensParams.t ->
        Lsp.Types.SemanticTokens.t option =
      fun ~notify_back:_ params ->
        Linol_eio.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Semantic_tokens.run params)

    method private on_req_references :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.ReferenceParams.t ->
        Lsp.Types.Location.t list option =
      fun ~notify_back:_ params ->
        Linol_eio.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Hover.find_references params)

    method private on_req_rename :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.RenameParams.t ->
        Lsp.Types.WorkspaceEdit.t =
      fun ~notify_back:_ params ->
        let result =
          Hashtbl.find buffers params.textDocument.uri |> Hover.rename params
        in
        let edit =
          match result with
          | Some edit -> edit
          | None ->
              {
                documentChanges = None;
                changes = None;
                changeAnnotations = None;
              }
        in
        let json = Lsp.Types.WorkspaceEdit.yojson_of_t edit in
        Log.info "Rename reply: %a" (Yojson.Safe.pretty_print ~std:true) json;
        Linol_eio.return edit

    method private on_req_prepare_rename :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.PrepareRenameParams.t ->
        Lsp.Types.Range.t option =
      fun ~notify_back:_ params ->
        Linol_eio.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Hover.prepare_rename params)

    method! on_request_unhandled : type r.
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        id:Linol.Server.Req_id.t ->
        r Lsp.Client_request.t ->
        r =
      fun ~notify_back ~id:_ request ->
        match request with
        | SelectionRange params ->
            self#on_req_selection_range ~notify_back params
        | TextDocumentFormatting params ->
            self#on_req_format ~notify_back params
        | TextDocumentReferences params ->
            self#on_req_references ~notify_back params
        | SemanticTokensFull params ->
            self#on_semantic_tokens ~notify_back params
        | TextDocumentRename params -> self#on_req_rename ~notify_back params
        | TextDocumentPrepareRename params ->
            self#on_req_prepare_rename ~notify_back params
        | _ -> Linol_eio.failwith "TODO handle this request"
  end

let run ({ dummy = () } : Args.t) =
  Eio_main.run (fun env ->
      Log.info "Starting Kast LSP";
      let s = new lsp_server in
      let server = Linol_eio.Jsonrpc2.create_stdio ~env s in
      let task () =
        let shutdown () = s#get_status = `ReceivedExit in
        Linol_eio.Jsonrpc2.run ~shutdown server
      in
      match task () with
      | () -> Log.info "Exiting Kast LSP"
      | exception e ->
          let e = Printexc.to_string e in
          Printf.eprintf "error: %s\n%!" e;
          exit 1)
