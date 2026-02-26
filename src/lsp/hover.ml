open Std
open Kast_util
open Kast_types
module Lsp = Linol_lsp
module Compiler = Kast_compiler

let rename_options : Lsp.Types.RenameOptions.t =
  { workDoneProgress = None; prepareProvider = Some true }
;;

let definition_options : Lsp.Types.DefinitionOptions.t = { workDoneProgress = None }
let references_options : Lsp.Types.ReferenceOptions.t = { workDoneProgress = None }

let options : Lsp.Types.HoverOptions.t =
  Lsp.Types.HoverOptions.create ?workDoneProgress:None ()
;;

type definition =
  { span : span
  ; references : span list
  }

type definition_mode =
  | DefinedHere of definition
  | DefinedNotHere of definition

let label_definition (label : Label.t) : definition =
  let data : Label.label_data = label |> Label.get_data in
  let span, references =
    match data.definition with
    | Some def -> def, data.references
    | None -> List.head data.references, List.tail data.references
  in
  { span; references }
;;

type hover_info_signature =
  { signature : ir_signature
  ; evaled : value option
  ; span : span
  }

type hover_info_rename =
  { definition_mode : definition_mode
  ; span : span
  }

type hover_info =
  { signature : hover_info_signature option
  ; rename : hover_info_rename option
  ; file : Uri.t option
  }

let hover_info_none = { signature = None; rename = None; file = None }

let combine_hover_info a b : hover_info =
  { file = a.file |> Option.or_ b.file
  ; rename =
      max a.rename b.rename
      (* Using max means that DefinedNotHere is preferred to DefinedHere
         which is important for things like `use foo` *)
  ; signature = a.signature |> Option.or_ b.signature
  }
;;

let hover_text info =
  match info.signature with
  | Some signature ->
    make_string "%t" (fun fmt ->
      fprintf fmt "```kast@\n";
      let { ty } : ir_signature = signature.signature in
      fprintf fmt "@[<v>:: %a@]\n" Print.print_ty_with_shape_if_named ty;
      (match signature.evaled with
       | None -> ()
       | Some value -> fprintf fmt "@[<v>= %a@]\n" Value.print value);
      fprintf fmt "```")
  | None -> ""
;;

let binding_definition : binding -> definition =
  fun binding -> label_definition binding.label
;;

let get_tuple (type a) (kind : a compiled_kind) (compiled : a) : a Types.tuple_of option =
  match kind with
  | PlaceExpr -> None
  | Expr ->
    (match compiled.shape with
     | E_Tuple tuple -> Some tuple
     | _ -> None)
  | Assignee ->
    (match compiled.shape with
     | A_Tuple tuple -> Some tuple
     | _ -> None)
  | TyExpr ->
    (match compiled.compiled_shape with
     | Some (TE_Tuple tuple) -> Some tuple
     | _ -> None)
  | Pattern ->
    (match compiled.shape with
     | P_Tuple tuple -> Some tuple
     | _ -> None)
;;

let hover_tuple : 'a. 'a compiled_kind -> 'a -> span -> hover_info option =
  fun (type a)
    (kind : a compiled_kind)
    (compiled : a)
    (hover_span : span)
    : hover_info option ->
  let* tuple = get_tuple kind compiled in
  tuple.parts
  |> List.find_map (fun (part : a Types.tuple_part_of) ->
    match part with
    | Unpack _ -> None
    | Field { label_span; label; field } ->
      if label_span |> Span.contains_span hover_span
      then (
        let data = Compiler.get_data kind field in
        Some
          { signature =
              Some { span = label_span; signature = data.signature; evaled = None }
          ; rename =
              label
              |> Option.map (fun label ->
                { span = label_span
                ; definition_mode =
                    (match kind with
                     | TyExpr -> DefinedHere (label_definition label)
                     | _ -> DefinedNotHere (label_definition label))
                })
          ; file = None
          })
      else None)
;;

let hover_specifically
  : 'a. 'a compiled_kind -> 'a -> evaled:value option -> span -> hover_info
  =
  fun (type a)
    (kind : a compiled_kind)
    (compiled : a)
    ~evaled:_
    (hover_span : span)
    : hover_info ->
  let data = Compiler.get_data kind compiled in
  let evaled = data.evaled.value in
  let included_file = data.included_file in
  match data.evaled.binding with
  | Some binding ->
    { signature = Some { signature = data.signature; span = data.span; evaled }
    ; rename =
        Some
          { span = binding.span
          ; definition_mode = DefinedNotHere (binding_definition binding)
          }
    ; file = included_file
    }
  | None ->
    (match hover_tuple kind compiled hover_span with
     | Some result -> result
     | None ->
       (match kind with
        | PlaceExpr ->
          let rename : hover_info_rename option =
            match compiled.shape with
            | PE_Binding binding ->
              Some
                { span = binding.span
                ; definition_mode = DefinedNotHere (binding_definition binding)
                }
            | PE_Field { obj = _; field; field_span } ->
              (match field with
               | Index _ -> None
               | Expr _ -> None
               | Name label ->
                 Some
                   { span = field_span
                   ; definition_mode = DefinedNotHere (label_definition label)
                   })
            | _ -> None
          in
          { signature =
              Some
                { signature = compiled.data.signature; span = compiled.data.span; evaled }
          ; rename
          ; file = included_file
          }
        | Expr ->
          let rename : hover_info_rename option =
            match compiled.shape with
            | E_Variant { label; label_span; value = _ } ->
              Some
                { span = label_span
                ; definition_mode = DefinedNotHere (label_definition label)
                }
            | _ -> None
          in
          { signature =
              Some
                { signature = compiled.data.signature; span = compiled.data.span; evaled }
          ; rename
          ; file = included_file
          }
        | Assignee ->
          { signature =
              Some
                { signature = compiled.data.signature; span = compiled.data.span; evaled }
          ; rename = None
          ; file = included_file
          }
        | Pattern ->
          let rename =
            match compiled.shape with
            | P_Binding { bind_mode = _; binding } ->
              Some
                { span = binding.span
                ; definition_mode = DefinedHere (binding_definition binding)
                }
            | P_Variant { label; label_span; value = _ } ->
              if label_span |> Span.contains_span hover_span
              then
                Some
                  { span = label_span
                  ; definition_mode = DefinedNotHere (label_definition label)
                  }
              else None
            | _ -> None
          in
          { signature =
              Some
                { signature = compiled.data.signature; span = compiled.data.span; evaled }
          ; rename
          ; file = included_file
          }
        | TyExpr ->
          let rename =
            match compiled.compiled_shape with
            | Some (TE_Variant { variants }) ->
              variants
              |> List.find_map
                   (fun
                       ({ label_span; label; value = _ } : Types.ty_expr_variant_variant)
                      ->
                      if label_span |> Span.contains_span hover_span
                      then
                        Some
                          { span = label_span
                          ; definition_mode = DefinedHere (label_definition label)
                          }
                      else None)
            | _ -> None
          in
          { signature =
              Some
                { signature = compiled.data.signature; span = compiled.data.span; evaled }
          ; rename
          ; file = included_file
          }))
;;

let rec hover : 'a. 'a compiled_kind -> 'a -> evaled:value option -> span -> hover_info =
  fun (type a)
    (kind : a compiled_kind)
    (compiled : a)
    ~(evaled : value option)
    (hover_span : span)
    : hover_info ->
  let data = Compiler.get_data kind compiled in
  let span = data.span in
  let inner =
    Common.inner_compiled kind compiled
    |> Seq.fold_left
         (fun acc (Types.Compiled (kind, inner), ~evaled) ->
            combine_hover_info acc (hover kind inner ~evaled hover_span))
         hover_info_none
  in
  if span |> Span.contains_span hover_span
  then combine_hover_info inner (hover_specifically kind compiled ~evaled hover_span)
  else inner
;;

let find_definition
      (pos : Lsp.Types.Position.t)
      ({ uri; compiled; _ } : Processing.file_state)
  : Lsp.Types.Locations.t option
  =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* (Compiled (kind, compiled)) = compiled in
  let hover_info = hover kind compiled ~evaled:None (Span.single_char pos uri) in
  match hover_info.file with
  | Some file ->
    let location : Lsp.Types.Location.t =
      { uri = Common.uri_to_lsp file
      ; range = Common.span_to_range (Span.beginning_of file)
      }
    in
    Some Lsp.Types.Locations.(`Location [ location ])
  | None ->
    let* definition =
      match hover_info.rename with
      | Some { definition_mode = DefinedNotHere definition; _ } -> Some definition
      | Some { definition_mode = DefinedHere _; _ } | None -> None
    in
    let location : Lsp.Types.Location.t =
      { uri = Common.uri_to_lsp definition.span.uri
      ; range = Common.span_to_range definition.span
      }
    in
    Some Lsp.Types.Locations.(`Location [ location ])
;;

let find_references
      (params : Lsp.Types.ReferenceParams.t)
      ({ uri; compiled; _ } : Processing.file_state)
  : Lsp.Types.Location.t list option
  =
  let pos : position = Common.lsp_to_kast_pos params.position in
  let* (Compiled (kind, compiled)) = compiled in
  let hover_info = hover kind compiled ~evaled:None (Span.single_char pos uri) in
  let* definition =
    match hover_info.rename with
    | Some { definition_mode = DefinedHere definition | DefinedNotHere definition; _ } ->
      Some definition
    | None -> None
  in
  let declaration_location =
    if params.context.includeDeclaration
    then Common.span_location definition.span
    else None
  in
  let references = definition.references |> List.filter_map Common.span_location in
  Some ((declaration_location |> Option.to_list) @ references)
;;

let prepare_rename
      (pos : Lsp.Types.Position.t)
      ({ uri; compiled; _ } : Processing.file_state)
  : Lsp.Types.Range.t option
  =
  let pos : position = Common.lsp_to_kast_pos pos in
  let* (Compiled (kind, compiled)) = compiled in
  let hover_info = hover kind compiled ~evaled:None (Span.single_char pos uri) in
  let* rename = hover_info.rename in
  Some (rename.span |> Common.span_to_range)
;;

exception Nope

let rename
      (position : Lsp.Types.Position.t)
      (newName : string)
      ({ uri; compiled; _ } : Processing.file_state)
  : Lsp.Types.WorkspaceEdit.t option
  =
  try
    let newText = Kast_lexer.maybe_convert_to_raw_ident newName in
    let pos : position = Common.lsp_to_kast_pos position in
    let* (Compiled (kind, compiled)) = compiled in
    let hover_info = hover kind compiled ~evaled:None (Span.single_char pos uri) in
    let* rename = hover_info.rename in
    let definition =
      match rename.definition_mode with
      | DefinedHere definition | DefinedNotHere definition -> definition
    in
    let spans = definition.span :: definition.references in
    Log.trace (fun log -> log "renaming spans: %a" (List.print Span.print) spans);
    let changes : Lsp.Types.TextEdit.t list UriMap.t =
      spans
      |> List.fold_left
           (fun changes (span : span) ->
              UriMap.update
                span.uri
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
      |> List.map (fun (uri, edits) -> Common.uri_to_lsp uri, edits)
    in
    let edit : Lsp.Types.WorkspaceEdit.t =
      { changes = Some changes
      ; (* documentChanges = Some [ `TextDocumentEdit changes ]; *)
        documentChanges = None
      ; changeAnnotations = None
      }
    in
    Some edit
  with
  | Nope -> None
;;

let hover (pos : Lsp.Types.Position.t) ({ uri; compiled; _ } : Processing.file_state)
  : Lsp.Types.Hover.t option
  =
  let pos = Common.lsp_to_kast_pos pos in
  Log.trace (fun log -> log "Hovering %a" Position.print pos);
  let* (Compiled (kind, compiled)) = compiled in
  let hover_info = hover kind compiled ~evaled:None (Span.single_char pos uri) in
  let hover_text = hover_text hover_info in
  Log.trace (fun log -> log "Hover result: %a" String.print_debug hover_text);
  let* ty = hover_info.signature in
  Some
    ({ contents = `MarkedString { language = None; value = hover_text }
     ; range = Some (Common.span_to_range ty.span)
     }
     : Lsp.Types.Hover.t)
;;
