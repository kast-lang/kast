open Std
open Kast_util
module Lsp = Linol_lsp

let options : Lsp.Types.DocumentFormattingOptions.t = { workDoneProgress = Some false }

let run (global : Processing.global_state) ({ uri; parsed; _ } : Processing.file_state)
  : Lsp.Types.TextEdit.t list option
  =
  Log.info (fun log -> log "got format request");
  if global.parse_errors |> Processing.UriSet.contains uri
  then None
  else (
    match parsed with
    | None -> None
    | Some parsed ->
      Kast_fmt.format Format.str_formatter parsed;
      let newText = Format.flush_str_formatter () in
      Some
        [ ({ newText
           ; range =
               { start = { line = 0; character = 0 }
               ; end_ = { line = 1000000000; character = 0 }
               }
           }
           : Lsp.Types.TextEdit.t)
        ])
;;
