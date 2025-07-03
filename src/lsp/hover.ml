open Std
open Kast_util
open Kast_types
module Compiler = Kast_compiler

type 'a compiled_kind = 'a Compiler.compiled_kind

let rename_options : Lsp.Types.RenameOptions.t =
  { workDoneProgress = None; prepareProvider = Some true }

let definition_options : Lsp.Types.DefinitionOptions.t =
  { workDoneProgress = None }

let references_options : Lsp.Types.ReferenceOptions.t =
  { workDoneProgress = None }

let options : Lsp.Types.HoverOptions.t =
  Lsp.Types.HoverOptions.create ?workDoneProgress:None ()

type definition = {
  span : span;
  references : span list;
}

type definition_mode =
  | DefinedHere of definition
  | DefinedNotHere of definition
  | None

type hover_info = {
  ty : ty;
  span : span;
  definition_mode : definition_mode;
}

let hover_text info = make_string "```kast\n%a\n```" Ty.print info.ty

let binding_definition : binding -> definition =
 fun binding -> { span = binding.span; references = binding.references }

let hover_specifially : 'a. 'a compiled_kind -> 'a -> hover_info =
 fun (type a) (kind : a compiled_kind) (compiled : a) : hover_info ->
  match kind with
  | Expr ->
      let definition_mode =
        match compiled.shape with
        | E_Binding binding -> DefinedNotHere (binding_definition binding)
        | _ -> None
      in
      { ty = compiled.data.ty; span = compiled.data.span; definition_mode }
  | Assignee ->
      let definition_mode =
        match compiled.shape with
        | A_Binding binding -> DefinedNotHere (binding_definition binding)
        | _ -> None
      in
      { ty = compiled.data.ty; span = compiled.data.span; definition_mode }
  | Pattern ->
      let definition_mode =
        match compiled.shape with
        | P_Binding binding -> DefinedHere (binding_definition binding)
        | _ -> None
      in
      { ty = compiled.data.ty; span = compiled.data.span; definition_mode }
  | TyExpr ->
      {
        ty = compiled.data.ty;
        span = compiled.data.span;
        definition_mode = None;
      }

let rec hover : 'a. 'a compiled_kind -> 'a -> position -> hover_info option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (pos : position) :
     hover_info option ->
  with_return (fun { return } ->
      let data = Compiler.get_data kind compiled in
      let span = data.span in
      let span_is_special =
        match span.filename with
        | Special _ -> true
        | _ -> false
      in
      (match data.ty_ascription with
      | None -> ()
      | Some ty_ascription_expr -> (
          match hover TyExpr ty_ascription_expr pos with
          | None -> ()
          | Some hover -> return (Some hover)));
      let* () =
        if span_is_special || span |> Span.contains pos then Some () else None
      in
      let inner : hover_info option =
        match kind with
        | Expr -> (
            match compiled.shape with
            | E_Constant _ -> None
            | E_Binding _ -> None
            | E_Then { a; b } ->
                hover Expr a pos |> Option.or_else (fun () -> hover Expr b pos)
            | E_Stmt { expr } -> hover Expr expr pos
            | E_Scope { expr } -> hover Expr expr pos
            | E_Fn { arg; body; evaled_result } ->
                hover Pattern arg pos
                |> Option.or_else (fun () -> hover Expr body pos)
                |> Option.or_else (fun () ->
                       evaled_result
                       |> Option.and_then (fun expr -> hover TyExpr expr pos))
            | E_Tuple { tuple } ->
                tuple |> Tuple.to_seq
                |> Seq.find_map (fun (_member, expr) -> hover Expr expr pos)
            | E_Apply { f; arg } ->
                hover Expr f pos
                |> Option.or_else (fun () -> hover Expr arg pos)
            | E_Assign { assignee; value } ->
                hover Assignee assignee pos
                |> Option.or_else (fun () -> hover Expr value pos)
            | E_Ty expr -> hover TyExpr expr pos
            | E_Native _ -> None
            | E_Module { def } -> hover Expr def pos
            | E_Field { obj; field = _ } -> hover Expr obj pos)
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
        | TyExpr -> (
            match compiled.shape with
            | TE_Unit -> None
            | TE_Fn { arg; result } ->
                hover TyExpr arg pos
                |> Option.or_else (fun () -> hover TyExpr result pos)
            | TE_Expr expr -> hover Expr expr pos)
      in
      match inner with
      | Some result -> Some result
      | None ->
          if span_is_special then None
          else Some (hover_specifially kind compiled))

let find_definition (pos : Lsp.Types.Position.t)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.Locations.t option =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let* definition =
    match hover_info.definition_mode with
    | DefinedNotHere definition -> Some definition
    | DefinedHere _ | None -> None
  in
  let* path = Path.as_file definition.span.filename in
  let location : Lsp.Types.Location.t =
    { uri = Lsp.Uri.of_path path; range = Common.span_to_range definition.span }
  in
  Some Lsp.Types.Locations.(`Location [ location ])

let find_references (params : Lsp.Types.ReferenceParams.t)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.Location.t list option
    =
  let pos : position = Common.lsp_to_kast_pos params.position in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let* definition =
    match hover_info.definition_mode with
    | DefinedHere definition | DefinedNotHere definition -> Some definition
    | None -> None
  in
  let declaration_location =
    if params.context.includeDeclaration then
      Common.span_location definition.span
    else None
  in
  let references =
    definition.references |> List.filter_map Common.span_location
  in
  Some ((declaration_location |> Option.to_list) @ references)

let prepare_rename (pos : Lsp.Types.Position.t)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.Range.t option =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let* _definition =
    match hover_info.definition_mode with
    | DefinedHere definition | DefinedNotHere definition -> Some definition
    | None -> None
  in
  Some (hover_info.span |> Common.span_to_range)

module UriMap = Map.Make (Lsp.Uri)

exception Nope

let rename (position : Lsp.Types.Position.t) (newName : string)
    ({ compiled; _ } : Processing.file_state) : Lsp.Types.WorkspaceEdit.t option
    =
  try
    (* TODO maybe convert into raw ident *)
    let newText = newName in
    let pos : position = Common.lsp_to_kast_pos position in
    let* expr = compiled in
    let* hover_info = hover Expr expr pos in
    let* definition =
      match hover_info.definition_mode with
      | DefinedHere definition | DefinedNotHere definition -> Some definition
      | None -> None
    in
    let spans = definition.span :: definition.references in
    let changes : Lsp.Types.TextEdit.t list UriMap.t =
      spans
      |> List.fold_left
           (fun changes (span : span) ->
             match Path.as_file span.filename with
             | None -> raise Nope
             | Some path ->
                 let uri = Lsp.Uri.of_path path in
                 UriMap.update uri
                   (fun current ->
                     let current =
                       match current with
                       | None -> []
                       | Some current -> current
                     in
                     let edit : Lsp.Types.TextEdit.t =
                       { range = span |> Common.span_to_range; newText }
                     in
                     Some (edit :: current))
                   changes)
           UriMap.empty
    in
    let changes : (Lsp.Uri.t * Lsp.Types.TextEdit.t list) list =
      UriMap.to_list changes
    in
    let edit : Lsp.Types.WorkspaceEdit.t =
      {
        changes = Some changes;
        (* documentChanges = Some [ `TextDocumentEdit changes ]; *)
        documentChanges = None;
        changeAnnotations = None;
      }
    in
    Some edit
  with Nope -> None

let hover (pos : Lsp.Types.Position.t) ({ compiled; _ } : Processing.file_state)
    : Lsp.Types.Hover.t option =
  let pos = Common.lsp_to_kast_pos pos in
  Log.info "Hovering %a" Position.print pos;
  let* expr = compiled in
  let* hover_info = hover Expr expr pos in
  let hover_text = hover_text hover_info in
  Log.info "Hover result: %S" hover_text;
  Some
    ({
       contents = `MarkedString { language = None; value = hover_text };
       range = Some (Common.span_to_range hover_info.span);
     }
      : Lsp.Types.Hover.t)
