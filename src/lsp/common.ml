open Std
open Kast_util

let span_to_range (span : span) : Lsp.Types.Range.t =
  {
    start = { line = span.start.line - 1; character = span.start.column - 1 };
    end_ = { line = span.finish.line - 1; character = span.finish.column - 1 };
  }

let lsp_to_kast_pos (pos : Lsp.Types.Position.t) : position =
  {
    (* We dont use index in lsp *)
    index = 0;
    line = pos.line + 1;
    column = pos.character + 1;
  }

let span_location (span : span) : Lsp.Types.Location.t option =
  let location : Lsp.Types.Location.t =
    {
      uri = Lsp.Uri.of_string (Uri.to_string span.uri);
      range = span_to_range span;
    }
  in
  Some location

let uri_to_lsp (uri : Uri.t) : Lsp.Uri.t = Lsp.Uri.of_string (Uri.to_string uri)

let uri_from_lsp (uri : Lsp.Uri.t) : Uri.t =
  Uri.of_string (Lsp.Uri.to_string uri)
