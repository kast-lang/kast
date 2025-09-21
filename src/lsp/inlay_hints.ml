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
    'a.
    uri:Uri.t -> 'a Compiler.compiled_kind -> 'a -> Lsp.Types.InlayHint.t Seq.t
    =
 fun (type a) ~(uri : Uri.t) (kind : a Compiler.compiled_kind) (compiled : a) ->
  let type_hint : ty option =
    match kind with
    | Expr -> None
    | Pattern -> (
        match compiled.shape with
        | P_Binding _ -> Some compiled.data.ty
        | _ -> None)
    | Assignee -> None
    | TyExpr -> None
  in
  let rest =
    Common.inner_compiled kind compiled
    |> Seq.flat_map (fun (Common.CompiledThing (kind, compiled)) ->
           inlay_hints ~uri kind compiled)
  in
  let data = Compiler.get_data kind compiled in
  let span = data.span in
  let type_hint =
    if data.ty_ascription |> Option.is_some then None
    else
      type_hint |> Option.map (fun ty -> make_string "@[<h>:: %a@]" Ty.print ty)
  in
  let hint : Lsp.Types.InlayHint.t option =
    if span.uri <> uri then None
    else
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
  let ascription =
    data.ty_ascription |> Option.to_seq
    |> Seq.flat_map (inlay_hints ~uri TyExpr)
  in
  rest |> Seq.append (hint |> Option.to_seq) |> Seq.append ascription

let inlay_hints ~uri (expr : expr) : Lsp.Types.InlayHint.t list =
  inlay_hints ~uri Expr expr |> List.of_seq

let get ({ uri; compiled; _ } : Processing.file_state) :
    Lsp.Types.InlayHint.t list option =
  Log.info (fun log -> log "got inlay hint req");
  let* expr = compiled in
  let hints = inlay_hints ~uri expr in
  Some hints
