open Std
open Kast_util
module Compiler = Kast_compiler
open Kast_types

let options : Lsp.Types.InlayHintRegistrationOptions.t =
  {
    workDoneProgress = None;
    resolveProvider = None;
    id = None;
    documentSelector = None;
  }

let rec inlay_hints :
    'a. 'a Compiler.compiled_kind -> 'a -> Lsp.Types.InlayHint.t Seq.t =
 fun (type a) (kind : a Compiler.compiled_kind) (compiled : a) ->
  let span, ((type_hint : string option), rest) =
    match kind with
    | Expr ->
        ( compiled.span,
          match compiled.shape with
          | E_Constant _ -> (None, Seq.empty)
          | E_Binding _ -> (None, Seq.empty)
          | E_Then { a; b } ->
              (None, Seq.append (inlay_hints Expr a) (inlay_hints Expr b))
          | E_Stmt { expr } -> (None, inlay_hints Expr expr)
          | E_Scope { expr } -> (None, inlay_hints Expr expr)
          | E_Fn { arg; body } ->
              ( None,
                Seq.append (inlay_hints Pattern arg) (inlay_hints Expr body) )
          | E_Tuple { tuple } ->
              ( None,
                tuple |> Tuple.to_seq
                |> Seq.flat_map (fun (_member, expr) -> inlay_hints Expr expr)
              )
          | E_Apply { f; arg } ->
              (None, Seq.append (inlay_hints Expr f) (inlay_hints Expr arg))
          | E_Assign { assignee; value } ->
              ( None,
                Seq.append
                  (inlay_hints Assignee assignee)
                  (inlay_hints Expr value) ) )
    | Pattern ->
        ( compiled.span,
          match compiled.shape with
          | P_Placeholder -> (None, Seq.empty)
          | P_Unit -> (None, Seq.empty)
          | P_Binding _ ->
              (Some (make_string ":: %a" Ty.print compiled.ty), Seq.empty) )
    | Assignee ->
        ( compiled.span,
          match compiled.shape with
          | A_Placeholder -> (None, Seq.empty)
          | A_Unit -> (None, Seq.empty)
          | A_Binding _ -> (None, Seq.empty)
          | A_Let pattern -> (None, inlay_hints Pattern pattern) )
  in
  let hint : Lsp.Types.InlayHint.t option =
    type_hint
    |> Option.map (fun type_hint : Lsp.Types.InlayHint.t ->
           {
             position =
               {
                 line = span.finish.line - 1;
                 character = span.finish.column - 1;
               };
             label = `String type_hint;
             kind = Some Type;
             textEdits = None;
             tooltip = None;
             paddingLeft = Some true;
             paddingRight = Some false;
             data = None;
           })
  in
  Seq.append (hint |> Option.to_seq) rest

let inlay_hints (expr : expr) : Lsp.Types.InlayHint.t list =
  inlay_hints Expr expr |> List.of_seq

let get (_range : Lsp.Types.Range.t) ({ compiled; _ } : Processing.file_state) :
    Lsp.Types.InlayHint.t list option =
  Log.info "got inlay hint req";
  let* expr = compiled in
  let hints = inlay_hints expr in
  Some hints
