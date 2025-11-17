open Std
open Kast_util
module Lsp = Linol_lsp
module Ast = Kast_ast

let options : Lsp.Types.SelectionRangeRegistrationOptions.t =
  { workDoneProgress = None; id = None; documentSelector = None }

let rec find_spans_start_biggest (ast : Ast.t) (pos : position) : span list =
  if Span.contains_position pos ast.span then
    ast.span
    ::
    (match ast.shape with
    | Error _ -> []
    | Simple _ -> []
    | Complex { root; _ } -> (
        let rec find_in_group ({ children; _ } : Ast.group) : span list option =
          children |> Tuple.to_seq
          |> Seq.find_map (fun (_member, (child : Ast.child)) ->
              match child with
              | Ast child ->
                  let child_spans = find_spans_start_biggest child pos in
                  if List.length child_spans = 0 then None else Some child_spans
              | Group child -> find_in_group child)
        in
        match find_in_group root with
        | None -> []
        | Some child_spans -> child_spans)
    | Syntax { value_after; _ } -> (
        match value_after with
        | None -> []
        | Some value -> find_spans_start_biggest value pos))
  else []

let get (params : Lsp.Types.SelectionRangeParams.t)
    ({ parsed; _ } : Processing.file_state) : Lsp.Types.SelectionRange.t list =
  Log.info (fun log -> log "got selection range request");
  match parsed with
  | Some { ast = Some ast; eof; _ } ->
      params.positions
      |> List.map (fun (pos : Lsp.Types.Position.t) ->
          let pos = Common.lsp_to_kast_pos pos in
          let full_file : span =
            {
              start = Position.beginning;
              finish = eof;
              uri = params.textDocument.uri |> Common.uri_from_lsp;
            }
          in
          let spans = full_file :: find_spans_start_biggest ast pos in
          Log.info (fun log -> log "SPANS: %a" (List.print Span.print) spans);
          spans
          |> List.fold_left
               (fun parent (span : span) ->
                 Some
                   ({ parent; range = span |> Common.span_to_range }
                     : Lsp.Types.SelectionRange.t))
               None
          |> Option.get)
  | Some { ast = None; _ } | None -> []
