open Std
open Kast_util
module Lsp = Linol_lsp

let get_for_file (global : Processing.global_state)
    (state : Processing.file_state) : Lsp.Types.Diagnostic.t list =
  global.diagnostics |> UriMap.find_opt state.uri |> function
  | Some list -> list
  | None -> []

let get (global : Processing.global_state) :
    (Lsp.Uri0.t * Lsp.Types.Diagnostic.t list) list =
  global.diagnostics |> UriMap.to_list
  |> List.map (fun (uri, diags) -> (Common.uri_to_lsp uri, diags))
