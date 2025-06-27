open Std
open Util

module Args = struct
  type args = { dummy : unit }
  type t = args

  let parse : string list -> args = function
    | [] -> { dummy = () }
    | arg :: _rest -> fail "Unexpected arg %S" arg
end

module Lsp = Linol.Lsp

type state_after_processing = { parsed : Parser.result option }

let process_some_input_file (source : source) : state_after_processing =
  let parsed =
    try
      let result = Parser.parse source Default_syntax.ruleset in
      Some result
    with _ -> None
  in
  { parsed }

module Tokens = struct
  type token_shape =
    | Keyword of Lexer.token
    | Value of Lexer.token
    | Comment of Lexer.Token.comment

  type token = {
    token : token_shape;
    span : span;
  }

  let rec collect : Ast.t -> token Seq.t =
   fun { shape; span = _ } ->
    match shape with
    | Ast.Simple { comments_before; token } ->
        Seq.append
          (comments_before |> List.to_seq
          |> Seq.map (fun (comment : Lexer.Token.comment spanned) ->
                 { token = Comment comment.value; span = comment.span }))
          (List.to_seq [ { token = Value token.value; span = token.span } ])
    | Ast.Complex { parts; _ } ->
        parts |> List.to_seq
        |> Seq.flat_map (function
             | Ast.Value ast -> collect ast
             | Ast.Keyword token ->
                 List.to_seq
                   [ { token = Keyword token.value; span = token.span } ]
             | Ast.Comment comment ->
                 List.to_seq
                   [ { token = Comment comment.value; span = comment.span } ])
end

let diagnostics (_state : state_after_processing) : Lsp.Types.Diagnostic.t list
    =
  []

let semanticTokensProvider =
  let legend =
    Lsp.Types.SemanticTokensLegend.create
      ~tokenTypes:
        [
          "namespace";
          "class";
          "enum";
          "interface";
          "struct";
          "typeParameter";
          "type";
          "parameter";
          "variable";
          "property";
          "enumMember";
          "decorator";
          "event";
          "function";
          "method";
          "macro";
          "label";
          "comment";
          "string";
          "keyword";
          "number";
          "regexp";
          "operator";
        ]
      ~tokenModifiers:
        [
          "declaration";
          "definition";
          "readonly";
          "static";
          "deprecated";
          "abstract";
          "async";
          "modification";
          "documentation";
          "defaultLibrary";
        ]
  in
  Lsp.Types.SemanticTokensRegistrationOptions.create ~full:(`Bool true) ~legend
    ()

module IO = Linol_lwt.IO_lwt

type linecol = {
  line : int;
  column : int;
}

let linecol (pos : position) : linecol =
  { line = pos.line; column = pos.column }

class lsp_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server

    method! config_modify_capabilities (c : Lsp.Types.ServerCapabilities.t) :
        Lsp.Types.ServerCapabilities.t =
      {
        c with
        documentFormattingProvider =
          Some (`DocumentFormattingOptions { workDoneProgress = Some false });
        semanticTokensProvider =
          Some (`SemanticTokensRegistrationOptions semanticTokensProvider);
      }

    (* one env per document *)
    val buffers : (Lsp.Types.DocumentUri.t, state_after_processing) Hashtbl.t =
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
        process_some_input_file
          { filename = File (Lsp.Types.DocumentUri.to_path uri); contents }
      in
      Hashtbl.replace buffers uri new_state;
      let diags = diagnostics new_state in
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

    method private on_format :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        Lsp.Types.DocumentFormattingParams.t ->
        Lsp.Types.TextEdit.t list option Lwt.t =
      fun ~notify_back:_ params ->
        Log.info "got format request";
        let { parsed; _ } = Hashtbl.find buffers params.textDocument.uri in
        match parsed with
        | None -> Linol_lwt.return None
        | Some parsed ->
            Kast_fmt.format Format.str_formatter parsed;
            let newText = Format.flush_str_formatter () in
            let result =
              Some
                [
                  ({
                     newText;
                     range =
                       {
                         start = { line = 0; character = 0 };
                         end_ = { line = 1000000000; character = 0 };
                       };
                   }
                    : Lsp.Types.TextEdit.t);
                ]
            in
            Linol_lwt.return result

    method private on_semantic_tokens :
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        Lsp.Types.SemanticTokensParams.t ->
        Lsp.Types.SemanticTokens.t option Lwt.t =
      fun ~notify_back:_ params ->
        Log.info "got semantic tokens request";
        let { parsed; _ } = Hashtbl.find buffers params.textDocument.uri in
        match parsed with
        | None -> Linol_lwt.return None
        | Some { ast; trailing_comments; eof = _ } ->
            let data =
              let prev_pos = ref Position.beginning in
              let tokens =
                Seq.append
                  (ast |> Option.to_seq
                  |> Seq.flat_map (fun ast -> ast |> Tokens.collect))
                  (trailing_comments |> List.to_seq
                  |> Seq.map
                       (fun
                         (comment : Lexer.Token.comment spanned)
                         :
                         Tokens.token
                       ->
                         { token = Comment comment.value; span = comment.span })
                  )
              in
              tokens
              |> Seq.flat_map (fun ({ token; span } : Tokens.token) ->
                     let pos = ref @@ linecol span.start in
                     Seq.of_dispenser (fun () ->
                         if !pos = linecol span.finish then None
                         else
                           let next_pos =
                             if !pos.line = span.finish.line then
                               linecol span.finish
                             else { line = !pos.line + 1; column = 1 }
                           in
                           (* we dont use index anyway *)
                           let fakepos (pos : linecol) : position =
                             { line = pos.line; column = pos.column; index = 0 }
                           in
                           let span : span =
                             {
                               start = fakepos !pos;
                               finish = fakepos next_pos;
                               filename = span.filename;
                             }
                           in
                           pos := next_pos;
                           Some span)
                     |> Seq.map (fun span : Tokens.token -> { token; span }))
              |> Seq.flat_map (fun ({ token; span } : Tokens.token) ->
                     let deltaLine = span.start.line - !prev_pos.line in
                     let deltaStartChar =
                       if deltaLine = 0 then
                         span.start.column - !prev_pos.column
                       else span.start.column - Position.beginning.column
                     in
                     let length =
                       if span.start.line = span.finish.line then
                         span.finish.column - span.start.column
                       else 1000 (* to the end of line :) *)
                     in
                     let tokenType =
                       match token with
                       | Tokens.Keyword _ -> Some 19 (* keyword *)
                       | Tokens.Value token -> (
                           match token with
                           | String _ -> Some 18 (* string *)
                           | Number _ -> Some 20
                           | _ -> None)
                       | Tokens.Comment _ -> Some 17
                     in
                     let tokenModifiers = 0 in
                     let data =
                       tokenType
                       |> Option.map (fun tokenType ->
                              [
                                deltaLine;
                                deltaStartChar;
                                length;
                                tokenType;
                                tokenModifiers;
                              ])
                     in
                     (match data with
                     | Some data ->
                         Log.trace "@[<h>data: %a %a@]" (List.print Int.print)
                           data Span.print span;
                         prev_pos := span.start
                     | None -> ());
                     let data = data |> Option.value ~default:[] in
                     List.to_seq data)
              |> Array.of_seq
            in
            let tokens =
              Lsp.Types.SemanticTokens.create ~data ?resultId:None ()
            in
            Log.info "replied with semantic tokens";
            Linol_lwt.return @@ Some tokens

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol.Server.Req_id.t ->
        r Lsp.Client_request.t ->
        r Lwt.t =
      fun ~notify_back ~id:_ request ->
        match request with
        | TextDocumentFormatting params -> self#on_format ~notify_back params
        | SemanticTokensFull params ->
            self#on_semantic_tokens ~notify_back params
        | _ -> IO.failwith "TODO handle this request"
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
