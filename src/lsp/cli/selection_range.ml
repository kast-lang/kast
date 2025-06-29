open Std
open Kast_util
module Ast = Kast_ast
module Lsp = Linol.Lsp

let options : Lsp.Types.SelectionRangeRegistrationOptions.t =
  { workDoneProgress = None; id = None; documentSelector = None }

let rec find_spans_start_biggest (ast : Ast.t) (pos : position) : span list =
  if Span.contains pos ast.span then
    ast.span
    ::
    (match ast.shape with
    | Simple _ -> []
    | Complex { children; _ } -> (
        let child_spans =
          children |> Tuple.to_seq
          |> Seq.find_map (fun (_member, child) ->
                 let child_spans = find_spans_start_biggest child pos in
                 if List.length child_spans = 0 then None else Some child_spans)
        in
        match child_spans with
        | None -> []
        | Some child_spans -> child_spans)
    | Syntax { value_after; _ } -> (
        match value_after with
        | None -> []
        | Some value -> find_spans_start_biggest value pos))
  else []

let get (params : Lsp.Types.SelectionRangeParams.t)
    ({ parsed; _ } : Processing.file_state) : Lsp.Types.SelectionRange.t list =
  Log.info "got selection range request";
  match parsed with
  | Some { ast = Some ast; eof; _ } ->
      params.positions
      |> List.map (fun (position : Lsp.Types.Position.t) ->
             let pos : position =
               {
                 (* we dont need it *)
                 index = 0;
                 line = position.line + 1;
                 column = position.character + 1;
               }
             in
             let full_file : span =
               {
                 start = Position.beginning;
                 finish = eof;
                 filename = File (params.textDocument.uri |> Lsp.Uri.to_path);
               }
             in
             let spans = full_file :: find_spans_start_biggest ast pos in
             Log.info "SPANS: %a" (List.print Span.print) spans;
             spans
             |> List.fold_left
                  (fun parent (span : span) ->
                    Some
                      ({ parent; range = span |> Common.span_to_range }
                        : Lsp.Types.SelectionRange.t))
                  None
             |> Option.get)
  | Some { ast = None; _ } | None -> []
