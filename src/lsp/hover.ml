open Std
open Kast_util
open Kast_types
module Lsp = Linol_lsp
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

type hover_info_ty = {
  ty : ty;
  span : span;
}

type hover_info_rename = {
  definition_mode : definition_mode;
  span : span;
}

type hover_info = {
  ty : hover_info_ty;
  rename : hover_info_rename option;
}

let hover_text info = make_string "```kast@\n@[<v>%a@]\n```" Ty.print info.ty.ty

let binding_definition : binding -> definition =
 fun binding -> { span = binding.span; references = binding.references }

let get_tuple (type a) (kind : a compiled_kind) (compiled : a) :
    (field_name_span:span * a) tuple option =
  match kind with
  | Expr -> (
      match compiled.shape with
      | E_Tuple { tuple } -> Some tuple
      | _ -> None)
  | Assignee -> (
      match compiled.shape with
      (* TODO | A_Tuple { tuple } -> Some tuple *)
      | _ -> None)
  | TyExpr -> (
      match compiled.shape with
      | TE_Tuple { tuple } -> Some tuple
      | _ -> None)
  | Pattern -> (
      match compiled.shape with
      | P_Tuple { tuple } -> Some tuple
      | _ -> None)

let hover_tuple : 'a. 'a compiled_kind -> 'a -> span -> hover_info option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (hover_span : span) :
     hover_info option ->
  let* tuple = get_tuple kind compiled in
  tuple |> Tuple.to_seq
  |> Seq.find_map (fun (member, (~field_name_span, field)) ->
      if field_name_span |> Span.contains_span hover_span then
        let data = Compiler.get_data kind field in
        Some
          {
            ty = { span = field_name_span; ty = data.ty };
            rename =
              Some
                {
                  span = field_name_span;
                  definition_mode =
                    (match kind with
                    | TyExpr ->
                        DefinedHere { span = field_name_span; references = [] }
                    | _ ->
                        DefinedNotHere
                          { span = field_name_span; references = [] });
                };
          }
      else None)

let hover_specifially : 'a. 'a compiled_kind -> 'a -> span -> hover_info =
 fun (type a) (kind : a compiled_kind) (compiled : a) (hover_span : span) :
     hover_info ->
  match hover_tuple kind compiled hover_span with
  | Some result -> result
  | None -> (
      match kind with
      | Expr ->
          let rename : hover_info_rename option =
            match compiled.shape with
            | E_Binding binding ->
                Some
                  {
                    span = binding.span;
                    definition_mode =
                      DefinedNotHere (binding_definition binding);
                  }
            | E_Field { obj; field } -> (
                let obj_shape =
                  obj.data.ty.var |> Kast_inference_base.Var.inferred_opt
                in
                match obj_shape with
                | None -> None
                | Some obj_shape -> (
                    match obj_shape with
                    | T_Tuple { tuple } -> (
                        match Tuple.get_named_opt field tuple with
                        | Some field ->
                            Some
                              {
                                span = field.span;
                                definition_mode =
                                  DefinedNotHere
                                    {
                                      span = field.span;
                                      references = [] (* TODO *);
                                    };
                              }
                        | None -> None)
                    | _ -> None))
            | _ -> None
          in
          { ty = { ty = compiled.data.ty; span = compiled.data.span }; rename }
      | Assignee ->
          let rename =
            match compiled.shape with
            | A_Binding binding ->
                Some
                  {
                    span = binding.span;
                    definition_mode =
                      DefinedNotHere (binding_definition binding);
                  }
            | _ -> None
          in
          { ty = { ty = compiled.data.ty; span = compiled.data.span }; rename }
      | Pattern ->
          let rename =
            match compiled.shape with
            | P_Binding binding ->
                Some
                  {
                    span = binding.span;
                    definition_mode = DefinedHere (binding_definition binding);
                  }
            | _ -> None
          in
          { ty = { ty = compiled.data.ty; span = compiled.data.span }; rename }
      | TyExpr ->
          {
            ty = { ty = compiled.data.ty; span = compiled.data.span };
            rename = None;
          })

let rec hover : 'a. 'a compiled_kind -> 'a -> span -> hover_info option =
 fun (type a) (kind : a compiled_kind) (compiled : a) (hover_span : span) :
     hover_info option ->
  let data = Compiler.get_data kind compiled in
  let span = data.span in
  let inner =
    Common.inner_compiled kind compiled
    |> Seq.find_map (fun (Common.CompiledThing (kind, inner)) ->
        hover kind inner hover_span)
  in
  match inner with
  | Some result -> Some result
  | None ->
      if span |> Span.contains_span hover_span then
        Some (hover_specifially kind compiled hover_span)
      else None

let find_definition (pos : Lsp.Types.Position.t)
    ({ uri; compiled; _ } : Processing.file_state) :
    Lsp.Types.Locations.t option =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr (Span.single_char pos uri) in
  let* definition =
    match hover_info.rename with
    | Some { definition_mode = DefinedNotHere definition; _ } -> Some definition
    | Some { definition_mode = DefinedHere _; _ } | None -> None
  in
  let location : Lsp.Types.Location.t =
    {
      uri = Common.uri_to_lsp definition.span.uri;
      range = Common.span_to_range definition.span;
    }
  in
  Some Lsp.Types.Locations.(`Location [ location ])

let find_references (params : Lsp.Types.ReferenceParams.t)
    ({ uri; compiled; _ } : Processing.file_state) :
    Lsp.Types.Location.t list option =
  let pos : position = Common.lsp_to_kast_pos params.position in
  let* expr = compiled in
  let* hover_info = hover Expr expr (Span.single_char pos uri) in
  let* definition =
    match hover_info.rename with
    | Some
        {
          definition_mode = DefinedHere definition | DefinedNotHere definition;
          _;
        } ->
        Some definition
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
    ({ uri; compiled; _ } : Processing.file_state) : Lsp.Types.Range.t option =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* expr = compiled in
  let* hover_info = hover Expr expr (Span.single_char pos uri) in
  let* rename = hover_info.rename in
  Some (rename.span |> Common.span_to_range)

exception Nope

let rename (position : Lsp.Types.Position.t) (newName : string)
    ({ uri; compiled; _ } : Processing.file_state) :
    Lsp.Types.WorkspaceEdit.t option =
  try
    let newText = Kast_lexer.maybe_convert_to_raw_ident newName in
    let pos : position = Common.lsp_to_kast_pos position in
    let* expr = compiled in
    let* hover_info = hover Expr expr (Span.single_char pos uri) in
    let* rename = hover_info.rename in
    let definition =
      match rename.definition_mode with
      | DefinedHere definition | DefinedNotHere definition -> definition
    in
    let spans = definition.span :: definition.references in
    let changes : Lsp.Types.TextEdit.t list UriMap.t =
      spans
      |> List.fold_left
           (fun changes (span : span) ->
             UriMap.update span.uri
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
    let changes : (Lsp.Uri0.t * Lsp.Types.TextEdit.t list) list =
      UriMap.to_list changes
      |> List.map (fun (uri, edits) -> (Common.uri_to_lsp uri, edits))
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

let hover (pos : Lsp.Types.Position.t)
    ({ uri; compiled; _ } : Processing.file_state) : Lsp.Types.Hover.t option =
  let pos = Common.lsp_to_kast_pos pos in
  Log.info (fun log -> log "Hovering %a" Position.print pos);
  let* expr = compiled in
  let* hover_info = hover Expr expr (Span.single_char pos uri) in
  let hover_text = hover_text hover_info in
  Log.info (fun log -> log "Hover result: %S" hover_text);
  Some
    ({
       contents = `MarkedString { language = None; value = hover_text };
       range = Some (Common.span_to_range hover_info.ty.span);
     }
      : Lsp.Types.Hover.t)
