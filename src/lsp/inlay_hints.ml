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
  let (type_hint : ty option), rest =
    match kind with
    | Expr -> (
        match compiled.shape with
        | E_Constant _ -> (None, Seq.empty)
        | E_Binding _ -> (None, Seq.empty)
        | E_Then { a; b } ->
            ( None,
              Seq.append (inlay_hints ~uri Expr a) (inlay_hints ~uri Expr b) )
        | E_Stmt { expr } -> (None, inlay_hints ~uri Expr expr)
        | E_Scope { expr } -> (None, inlay_hints ~uri Expr expr)
        | E_Fn { arg; body; evaled_result } ->
            ( None,
              [
                inlay_hints ~uri Pattern arg;
                inlay_hints ~uri Expr body;
                evaled_result |> Option.to_seq
                |> Seq.flat_map (inlay_hints ~uri TyExpr);
              ]
              |> List.to_seq |> Seq.concat )
        | E_Tuple { tuple } ->
            ( None,
              tuple |> Tuple.to_seq
              |> Seq.flat_map (fun (_member, expr) ->
                     inlay_hints ~uri Expr expr) )
        | E_Apply { f; arg } ->
            ( None,
              Seq.append (inlay_hints ~uri Expr f) (inlay_hints ~uri Expr arg)
            )
        | E_Assign { assignee; value } ->
            ( None,
              Seq.append
                (inlay_hints ~uri Assignee assignee)
                (inlay_hints ~uri Expr value) )
        | E_Ty expr -> (None, inlay_hints ~uri TyExpr expr)
        | E_Native _ -> (None, Seq.empty)
        | E_Module { def } -> (None, inlay_hints ~uri Expr def)
        | E_Field { obj; field = _ } -> (None, inlay_hints ~uri Expr obj)
        | E_UseDotStar { used; bindings = _ } ->
            (None, inlay_hints ~uri Expr used)
        | E_If { cond; then_case; else_case } ->
            ( None,
              [
                inlay_hints ~uri Expr cond;
                inlay_hints ~uri Expr then_case;
                inlay_hints ~uri Expr else_case;
              ]
              |> List.to_seq |> Seq.concat )
        | E_Loop { body } -> (None, inlay_hints ~uri Expr body)
        | E_QuoteAst _ ->
            (* TODO *)
            (None, Seq.empty)
        | E_Error -> (None, Seq.empty))
    | Pattern -> (
        match compiled.shape with
        | P_Placeholder -> (None, Seq.empty)
        | P_Unit -> (None, Seq.empty)
        | P_Binding _ -> (Some compiled.data.ty, Seq.empty)
        | P_Tuple { tuple } ->
            ( None,
              tuple |> Tuple.to_seq
              |> Seq.flat_map (fun (_member, field_pattern) ->
                     inlay_hints ~uri Pattern field_pattern) )
        | P_Error -> (None, Seq.empty))
    | Assignee -> (
        match compiled.shape with
        | A_Placeholder -> (None, Seq.empty)
        | A_Unit -> (None, Seq.empty)
        | A_Binding _ -> (None, Seq.empty)
        | A_Let pattern -> (None, inlay_hints ~uri Pattern pattern)
        | A_Error -> (None, Seq.empty))
    | TyExpr -> (
        match compiled.shape with
        | TE_Unit -> (None, Seq.empty)
        | TE_Fn { arg; result } ->
            ( None,
              Seq.append
                (inlay_hints ~uri TyExpr arg)
                (inlay_hints ~uri TyExpr result) )
        | TE_Expr expr -> (None, inlay_hints ~uri Expr expr)
        | TE_Tuple { tuple } ->
            ( None,
              tuple |> Tuple.to_seq
              |> Seq.flat_map (fun (_member, expr) ->
                     inlay_hints ~uri TyExpr expr) )
        | TE_Error -> (None, Seq.empty))
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
