open Std
open Kast_util

let get (state : Processing.file_state) : Lsp.Types.Diagnostic.t list =
  state.diagnostics
