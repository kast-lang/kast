open Std
open Kast_util
open Kast_types
module Compiler = Kast_compiler

type 'a compiled_kind = 'a Compiler.compiled_kind

let definition_options : Lsp.Types.DefinitionOptions.t =
  { workDoneProgress = None }

let options : Lsp.Types.HoverOptions.t =
  Lsp.Types.HoverOptions.create ?workDoneProgress:None ()

type definition = { span : span }

type hover_info = {
  ty : ty;
  span : span;
  definition : definition option;
}

let hover_text info = make_string "```kast\n%a\n```" Ty.print info.ty

let binding_definition : binding -> definition =
 fun binding -> { span = binding.span }

let hover_specifially : 'a. 'a compiled_kind -> 'a -> hover_info =
 fun (type a) (kind : a compiled_kind) (compiled : a) : hover_info ->
  match kind with
  | Expr ->
      let definition =
        match compiled.shape with
        | E_Binding binding -> Some (binding_definition binding)
        | _ -> None
      in
      { ty = compiled.ty; span = compiled.span; definition }
  | Assignee ->
      let definition =
        match compiled.shape with
        | A_Binding binding -> Some (binding_definition binding)
        | _ -> None
      in
      { ty = compiled.ty; span = compiled.span; definition }
  | Pattern -> { ty = compiled.ty; span = compiled.span; definition = None }

let rec hover : 'a. 'a compiled_kind -> 'a -> position -> hover_info option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (pos : position) :
     hover_info option ->
  let span = Compiler.get_span kind compiled in
  let span_is_special =
    match span.filename with
    | Special _ -> true
    | _ -> false
  in
  let* () =
    if span_is_special || span |> Span.contains pos then Some () else None
  in
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
  | None ->
      if span_is_special then None else Some (hover_specifially kind compiled)

let find_defitinition (pos : Lsp.Types.Position.t)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.Locations.t option =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let* definition = hover_info.definition in
  let* path = Path.as_file definition.span.filename in
  let location : Lsp.Types.Location.t =
    { uri = Lsp.Uri.of_path path; range = Common.span_to_range definition.span }
  in
  Some Lsp.Types.Locations.(`Location [ location ])

let hover (pos : Lsp.Types.Position.t) ({ compiled; _ } : Processing.file_state)
    : Lsp.Types.Hover.t option =
  let pos = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let hover_text = hover_text hover_info in
  Some
    ({
       contents = `MarkedString { language = None; value = hover_text };
       range = Some (Common.span_to_range hover_info.span);
     }
      : Lsp.Types.Hover.t)
