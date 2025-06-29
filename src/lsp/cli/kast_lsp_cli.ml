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
    inherit Linol_lwt.Jsonrpc2.server

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
      }

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, Processing.file_state) Hashtbl.t =
      Hashtbl.create 32

    method spawn_query_handler f = Linol_lwt.spawn f

    (* We define here a helper method that will:
       - process a document
       - store the state resulting from the processing
       - return the diagnostics from the new state
    *)
    method private _on_doc ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        (uri : Lsp.Types.DocumentUri.t) (contents : string) =
      Log.info "processing file %S" (Lsp.Uri.to_path uri);

      let new_state =
        Processing.process_file
          { filename = File (Lsp.Types.DocumentUri.to_path uri); contents }
      in
      Hashtbl.replace buffers uri new_state;
      let diags = Diagnostics.get new_state in
      notify_back#send_diagnostic diags

    (* We now override the [on_notify_doc_did_open] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_open ~notify_back d ~content : unit Linol_lwt.t =
      self#_on_doc ~notify_back d.uri content

    (* Similarly, we also override the [on_notify_doc_did_change] method that will be called
       by the server each time a new document is opened. *)
    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old
        ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    (* On document closes, we remove the state associated to the file from the global
       hashtable state, to avoid leaking memory. *)
    method on_notif_doc_did_close ~notify_back:_ d : unit Linol_lwt.t =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.return ()

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri
        ~(range : Lsp.Types.Range.t) () :
        Lsp.Types.InlayHint.t list option Linol_lwt.t =
      Linol_lwt.return (Hashtbl.find buffers uri |> Inlay_hints.get range)

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        (_ : Linol_lwt.doc_state) : Lsp.Types.Hover.t option Linol_lwt.t =
      Linol_lwt.return (Hashtbl.find buffers uri |> Hover.run pos)

    method private on_selection_range :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        Lsp.Types.SelectionRangeParams.t ->
        Lsp.Types.SelectionRange.t list Lwt.t =
      fun ~notify_back:_ params ->
        Linol_lwt.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Selection_range.get params)

    method private on_format :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        Lsp.Types.DocumentFormattingParams.t ->
        Lsp.Types.TextEdit.t list option Lwt.t =
      fun ~notify_back:_ params ->
        Linol_lwt.return
          (Hashtbl.find buffers params.textDocument.uri |> Formatting.run params)

    method private on_semantic_tokens :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        Lsp.Types.SemanticTokensParams.t ->
        Lsp.Types.SemanticTokens.t option Lwt.t =
      fun ~notify_back:_ params ->
        Linol_lwt.return
          (Hashtbl.find buffers params.textDocument.uri
          |> Semantic_tokens.run params)

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol.Server.Req_id.t ->
        r Lsp.Client_request.t ->
        r Lwt.t =
      fun ~notify_back ~id:_ request ->
        match request with
        | SelectionRange params -> self#on_selection_range ~notify_back params
        | TextDocumentFormatting params -> self#on_format ~notify_back params
        | SemanticTokensFull params ->
            self#on_semantic_tokens ~notify_back params
        | _ -> Linol_lwt.failwith "TODO handle this request"
  end

let run ({ dummy = () } : Args.t) =
  Log.info "Starting Kast LSP";
  let s = new lsp_server in
  let server = Linol_lwt.Jsonrpc2.create_stdio ~env:() s in
  let task =
    let shutdown () = s#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown server
  in
  match Linol_lwt.run task with
  | () ->
      Log.info "Exiting Kast LSP";
      ()
  | exception e -> raise e
