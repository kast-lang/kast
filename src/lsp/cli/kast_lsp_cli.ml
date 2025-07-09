open Std
open Kast_util
module Compiler = Kast_compiler
module Token = Kast_token
module Lexer = Kast_lexer
module Ast = Kast_ast
module Parser = Kast_parser
module Processing = Kast_lsp.Processing
open Kast_types

module Args = struct
  type args = { dummy : unit }
  type t = args

  let parse : string list -> args = function
    | [] -> { dummy = () }
    | arg :: _rest -> fail "Unexpected arg %S" arg
end

class lsp_server ~(sw : Eio.Switch.t) ~domain_mgr =
  object (self)
    inherit Linol_eio.Jsonrpc2.server as super
    val updates : unit Latest_state.t = Latest_state.init ~sw
    val state : Processing.global_state option ref = ref None

    method private get_state () =
      match !state with
      | Some state -> state
      | None -> fail "Trying to get state before initialize"

    method private file_state uri =
      updates |> Latest_state.get_latest_result |> ignore;
      Processing.file_state (self#get_state ()) uri

    method! on_req_initialize ~notify_back (i : Lsp.Types.InitializeParams.t) :
        Lsp.Types.InitializeResult.t =
      let workspace_folders =
        i.workspaceFolders |> Option.to_seq |> Seq.flat_map Option.to_seq
        |> Seq.flat_map List.to_seq
      in
      state :=
        Some
          (Processing.init
             (workspace_folders
             |> Seq.map (fun (workspace : Lsp.Types.WorkspaceFolder.t) ->
                    workspace.uri)
             |> List.of_seq));
      super#on_req_initialize ~notify_back i

    method! config_modify_capabilities (c : Lsp.Types.ServerCapabilities.t) :
        Lsp.Types.ServerCapabilities.t =
      {
        c with
        documentFormattingProvider =
          Some (`DocumentFormattingOptions Kast_lsp.Formatting.options);
        semanticTokensProvider =
          Some
            (`SemanticTokensRegistrationOptions Kast_lsp.Semantic_tokens.options);
        selectionRangeProvider =
          Some
            (`SelectionRangeRegistrationOptions Kast_lsp.Selection_range.options);
        inlayHintProvider =
          Some (`InlayHintRegistrationOptions Kast_lsp.Inlay_hints.options);
        hoverProvider = Some (`HoverOptions Kast_lsp.Hover.options);
        referencesProvider =
          Some (`ReferenceOptions Kast_lsp.Hover.references_options);
        definitionProvider =
          Some (`DefinitionOptions Kast_lsp.Hover.definition_options);
        renameProvider = Some (`RenameOptions Kast_lsp.Hover.rename_options);
      }

    method spawn_query_handler f = Linol_eio.spawn f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(changed : bool)
        ~(notify_back : Linol_eio.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      Log.info "_on_doc %S" (Lsp.Uri.to_path uri);

      if changed then
        updates
        |> Latest_state.spawn ~domain_mgr (fun () ->
               Processing.update_file (self#get_state ()) uri contents);

      Eio.Fiber.fork ~sw (fun () ->
          let new_state = self#file_state uri in
          let diags =
            match new_state with
            | Some new_state -> Kast_lsp.Diagnostics.get new_state
            | None -> []
          in
          let print_diag fmt diag =
            fprintf fmt "%s"
              (diag |> Lsp.Types.Diagnostic.yojson_of_t |> Yojson.Safe.to_string)
          in
          Log.info "sending diags %a" (List.print print_diag) diags;
          notify_back#send_diagnostic diags)

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit =
      self#_on_doc ~changed:false ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~changed:true ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ _ : unit = ()

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri
        ~(range : Lsp.Types.Range.t) () : Lsp.Types.InlayHint.t list option =
      let _ = range in
      let* file_state = self#file_state uri in
      file_state |> Kast_lsp.Inlay_hints.get

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        (_ : Linol_eio.doc_state) : Lsp.Types.Hover.t option =
      let* file_state = self#file_state uri in
      file_state |> Kast_lsp.Hover.hover pos

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ (_ : Linol_eio.doc_state) :
        Lsp.Types.Locations.t option =
      let* file_state = self#file_state uri in
      file_state |> Kast_lsp.Hover.find_definition pos

    method private on_req_selection_range :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.SelectionRangeParams.t ->
        Lsp.Types.SelectionRange.t list =
      fun ~notify_back:_ params ->
        match self#file_state params.textDocument.uri with
        | Some file_state -> file_state |> Kast_lsp.Selection_range.get params
        | None -> []

    method private on_req_format :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.DocumentFormattingParams.t ->
        Lsp.Types.TextEdit.t list option =
      fun ~notify_back:_ params ->
        let* file_state = self#file_state params.textDocument.uri in
        file_state |> Kast_lsp.Formatting.run

    method private on_semantic_tokens :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.SemanticTokensParams.t ->
        Lsp.Types.SemanticTokens.t option =
      fun ~notify_back:_ params ->
        let* file_state = self#file_state params.textDocument.uri in
        file_state |> Kast_lsp.Semantic_tokens.run

    method private on_req_references :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.ReferenceParams.t ->
        Lsp.Types.Location.t list option =
      fun ~notify_back:_ params ->
        let* file_state = self#file_state params.textDocument.uri in
        file_state |> Kast_lsp.Hover.find_references params

    method private on_req_rename :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.RenameParams.t ->
        Lsp.Types.WorkspaceEdit.t =
      fun ~notify_back:_ params ->
        let result =
          let* file_state = self#file_state params.textDocument.uri in
          file_state |> Kast_lsp.Hover.rename params.position params.newName
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
        edit

    method private on_req_prepare_rename :
        notify_back:Linol_eio.Jsonrpc2.notify_back ->
        Lsp.Types.PrepareRenameParams.t ->
        Lsp.Types.Range.t option =
      fun ~notify_back:_ params ->
        let* file_state = self#file_state params.textDocument.uri in
        file_state |> Kast_lsp.Hover.prepare_rename params.position

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
      Eio.Switch.run (fun sw ->
          Log.info "Starting Kast LSP";
          let s = new lsp_server ~sw ~domain_mgr:(Eio.Stdenv.domain_mgr env) in
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
              exit 1))
