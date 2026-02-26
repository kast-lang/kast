open Std
open Kast_util
module Lsp = Linol_lsp
module Compiler = Kast_compiler
open Kast_types

let options : Lsp.Types.InlayHintRegistrationOptions.t =
  { workDoneProgress = None; resolveProvider = None; id = None; documentSelector = None }
;;

let rec inlay_hints
  : 'a. uri:Uri.t -> 'a compiled_kind -> 'a -> Lsp.Types.InlayHint.t Seq.t
  =
  fun (type a) ~(uri : Uri.t) (kind : a compiled_kind) (compiled : a) ->
  let type_hint : ty option =
    match kind with
    | PlaceExpr -> None
    | Expr -> None
    | Pattern ->
      (match compiled.shape with
       | P_Binding _ -> Some compiled.data.signature.ty
       | _ -> None)
    | Assignee -> None
    | TyExpr -> None
  in
  let rest =
    Common.inner_compiled kind compiled
    |> Seq.flat_map (fun (Types.Compiled (kind, compiled), ~evaled:_) ->
      inlay_hints ~uri kind compiled)
  in
  let data = Compiler.get_data kind compiled in
  let span = data.span in
  let type_hint =
    if data.evaled.ty_ascribed
    then None
    else type_hint |> Option.map (fun ty -> make_string "@[<h>:: %a@]" Ty.print ty)
  in
  let hint : Lsp.Types.InlayHint.t option =
    if span.uri <> uri
    then None
    else
      type_hint
      |> Option.map (fun type_hint : Lsp.Types.InlayHint.t ->
        { position = { line = span.finish.line - 1; character = span.finish.column - 1 }
        ; label = `String type_hint
        ; kind = Some Type
        ; textEdits = None
        ; tooltip = None
        ; paddingLeft = Some true
        ; paddingRight = Some false
        ; data = None
        })
  in
  let evaled =
    let { exprs; ty_exprs; patterns; ty_ascribed = _; value = _; binding = _ }
      : Types.ir_evaled
      =
      data.evaled
    in
    let exprs =
      exprs |> List.to_seq |> Seq.map (fun (expr, _value) -> Types.Compiled (Expr, expr))
    in
    let ty_exprs =
      ty_exprs
      |> List.to_seq
      |> Seq.map (fun (ty_expr, _ty) -> Types.Compiled (TyExpr, ty_expr))
    in
    let patterns =
      patterns
      |> List.to_seq
      |> Seq.map (fun pattern -> Types.Compiled (Pattern, pattern))
    in
    [ exprs; ty_exprs; patterns ]
    |> List.to_seq
    |> Seq.flat_map
         (Seq.flat_map (fun (Types.Compiled (kind, compiled)) ->
            inlay_hints ~uri kind compiled))
  in
  rest |> Seq.append (hint |> Option.to_seq) |> Seq.append evaled
;;

let get ({ uri; compiled; _ } : Processing.file_state) : Lsp.Types.InlayHint.t list option
  =
  Log.trace (fun log -> log "got inlay hint req");
  let* (Compiled (kind, compiled)) = compiled in
  let hints = inlay_hints ~uri kind compiled |> List.of_seq in
  Some hints
;;
