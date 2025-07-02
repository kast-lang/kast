open Std
open Kast_util

let get (state : Processing.file_state) : Lsp.Types.Diagnostic.t list =
  let parser_error =
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
  let compiler_error =
    let* compiler_error = state.compiler_error in
    Some
      ({
         range = compiler_error.span |> Common.span_to_range;
         severity = Some Error;
         code = None;
         codeDescription = None;
         source = None;
         message =
           `String (make_string "%a" (fun fmt () -> compiler_error.msg fmt) ());
         tags = None;
         relatedInformation = None;
         data = None;
       }
        : Lsp.Types.Diagnostic.t)
  in
  let type_error =
    let* type_error = state.type_error in
    Some
      ({
         range = type_error.span |> Common.span_to_range;
         severity = Some Error;
         code = None;
         codeDescription = None;
         source = None;
         message =
           `String (make_string "%a" (fun fmt () -> type_error.msg fmt) ());
         tags = None;
         relatedInformation = None;
         data = None;
       }
        : Lsp.Types.Diagnostic.t)
  in
  (parser_error |> Option.to_list)
  @ (compiler_error |> Option.to_list)
  @ (type_error |> Option.to_list)
