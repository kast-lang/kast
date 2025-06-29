open Std
open Kast_util
open Kast_types

type 'a compiled_kind = 'a Kast_compiler.compiled_kind

let options : Lsp.Types.HoverOptions.t =
  Lsp.Types.HoverOptions.create ?workDoneProgress:None ()

type hover_info = {
  ty : ty;
  span : span;
}

let hover_text info = make_string "```kast\n%a\n```" Ty.print info.ty

let data : 'a. 'a compiled_kind -> 'a -> span * ty =
 fun (type a) (kind : a compiled_kind) (compiled : a) ->
  match kind with
  | Expr -> (compiled.span, compiled.ty)
  | Assignee -> (compiled.span, compiled.ty)
  | Pattern -> (compiled.span, compiled.ty)

let rec hover : 'a. 'a compiled_kind -> 'a -> position -> hover_info option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (pos : position) :
     hover_info option ->
  let span, ty = data kind compiled in
  if span |> Span.contains pos then
    let inner =
      match kind with
      | Expr -> (
          match compiled.shape with
          | E_Constant _ -> None
          | E_Binding _ -> None
          | E_Then { a; b } ->
              hover Expr a pos |> Option.or_else (fun () -> hover Expr b pos)
          | E_Stmt { expr } -> hover Expr expr pos
          | E_Scope { expr } -> hover Expr expr pos
          | E_Fn { arg; body } ->
              hover Pattern arg pos
              |> Option.or_else (fun () -> hover Expr body pos)
          | E_Tuple { tuple } ->
              tuple |> Tuple.to_seq
              |> Seq.find_map (fun (_member, expr) -> hover Expr expr pos)
          | E_Apply { f; arg } ->
              hover Expr f pos |> Option.or_else (fun () -> hover Expr arg pos)
          | E_Assign { assignee; value } ->
              hover Assignee assignee pos
              |> Option.or_else (fun () -> hover Expr value pos))
      | Assignee -> (
          match compiled.shape with
          | A_Placeholder -> None
          | A_Unit -> None
          | A_Binding _ -> None
          | A_Let pattern -> hover Pattern pattern pos)
      | Pattern -> (
          match compiled.shape with
          | P_Placeholder -> None
          | P_Unit -> None
          | P_Binding _ -> None)
    in
    match inner with
    | Some result -> Some result
    | None -> Some { span; ty }
  else None

let run (pos : Lsp.Types.Position.t) ({ compiled; _ } : Processing.file_state) :
    Lsp.Types.Hover.t option =
  let pos : position =
    { index = 0; line = pos.line + 1; column = pos.character + 1 }
  in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let hover_text = hover_text hover_info in
  Some
    ({
       contents = `MarkedString { language = None; value = hover_text };
       range = Some (Common.span_to_range hover_info.span);
     }
      : Lsp.Types.Hover.t)
