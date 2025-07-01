open Std
open Kast_util

let get (state : Processing.file_state) : Lsp.Types.Diagnostic.t list =
  let d =
    let* parser_error = state.parser_error in
    Some
      ({
         range = parser_error.span |> Common.span_to_range;
         severity = Some Error;
         code = None;
         codeDescription = None;
         source = None;
         message =
           `String (make_string "%a" (fun fmt () -> parser_error.msg fmt) ());
         tags = None;
         relatedInformation = None;
         data = None;
       }
        : Lsp.Types.Diagnostic.t)
  in
  d |> Option.to_list
