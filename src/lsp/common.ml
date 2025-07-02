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
  let* path = Path.as_file span.filename in
  let location : Lsp.Types.Location.t =
    { uri = Lsp.Uri.of_path path; range = span_to_range span }
  in
  Some location
