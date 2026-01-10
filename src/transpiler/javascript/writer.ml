open Std
open Kast_util

type t =
  { fmt : formatter
  ; source_map_path : string
  ; mutable pos : position
  ; mutable span_start : position option
  ; mutable sources : Uri.t list
  ; mutable names : string list
  ; mutable name_idx : int StringMap.t
  ; mutable source_idx : int UriMap.t
  ; mappings_buffer : Buffer.t
  ; mappings_fmt : formatter
  ; mutable prev_source_idx : int option
  ; mutable prev_mapping_column : int option
  ; mutable prev_source_line : int option
  ; mutable prev_source_column : int option
  ; mutable prev_name_idx : int option
  ; mutable line_prefix : string
  }

let add_to_source_map
      (gen_pos : position)
      (orig_uri : Uri.t)
      (orig_pos : position)
      ~(name : string option)
      (writer : t)
  : unit
  =
  Log.trace (fun log ->
    log "ADD TO SOURCE MAP %a %a" Position.print gen_pos Position.print orig_pos);
  if orig_uri |> Uri.path |> String.ends_with ~suffix:"guess-a-number.ks"
  then println "%a:%a" Uri.print orig_uri Position.print orig_pos;
  let source_idx =
    match writer.source_idx |> UriMap.find_opt orig_uri with
    | Some idx -> idx
    | None ->
      let idx = writer.sources |> List.length in
      writer.sources <- List.append writer.sources [ orig_uri ];
      writer.source_idx <- writer.source_idx |> UriMap.add orig_uri idx;
      idx
  in
  if gen_pos.line = writer.pos.line
  then (
    let col =
      match writer.prev_mapping_column with
      | None -> gen_pos.column - 1
      | Some prev ->
        fprintf writer.mappings_fmt ",";
        let relative = gen_pos.column - 1 - prev in
        if relative <= 0 then fail "relative <= 0";
        relative
    in
    writer.prev_mapping_column <- Some (gen_pos.column - 1);
    Base64_vlq.print_base64_vlq writer.mappings_fmt col;
    (match writer.prev_source_idx with
     | None -> Base64_vlq.print_base64_vlq writer.mappings_fmt source_idx
     | Some prev -> Base64_vlq.print_base64_vlq writer.mappings_fmt (source_idx - prev));
    writer.prev_source_idx <- Some source_idx;
    (match writer.prev_source_line with
     | None -> Base64_vlq.print_base64_vlq writer.mappings_fmt (orig_pos.line - 1)
     | Some prev ->
       Base64_vlq.print_base64_vlq writer.mappings_fmt (orig_pos.line - 1 - prev));
    writer.prev_source_line <- Some (orig_pos.line - 1);
    (match writer.prev_source_column with
     | None -> Base64_vlq.print_base64_vlq writer.mappings_fmt (orig_pos.column - 1)
     | Some prev ->
       Base64_vlq.print_base64_vlq writer.mappings_fmt (orig_pos.column - 1 - prev));
    writer.prev_source_column <- Some (orig_pos.column - 1);
    match name with
    | None -> ()
    | Some name ->
      let name_idx =
        match writer.name_idx |> StringMap.find_opt name with
        | Some idx -> idx
        | None ->
          let idx = writer.names |> List.length in
          writer.names <- List.append writer.names [ name ];
          writer.name_idx <- writer.name_idx |> StringMap.add name idx;
          idx
      in
      (match writer.prev_name_idx with
       | None -> Base64_vlq.print_base64_vlq writer.mappings_fmt name_idx
       | Some prev -> Base64_vlq.print_base64_vlq writer.mappings_fmt (name_idx - prev));
      writer.prev_name_idx <- Some name_idx)
;;

let init fmt source_map_path =
  let mappings_buffer = Buffer.create 32 in
  { fmt
  ; source_map_path
  ; pos = Position.beginning
  ; span_start = None
  ; sources = []
  ; source_idx = UriMap.empty
  ; names = []
  ; name_idx = StringMap.empty
  ; mappings_buffer
  ; mappings_fmt = Format.formatter_of_buffer mappings_buffer
  ; prev_mapping_column = None
  ; prev_source_idx = None
  ; prev_source_line = None
  ; prev_source_column = None
  ; prev_name_idx = None
  ; line_prefix = ""
  }
;;

let finish (writer : t) : unit =
  Format.pp_print_flush writer.mappings_fmt ();
  let mappings = writer.mappings_buffer |> Buffer.contents in
  let out = open_out writer.source_map_path in
  let fmt = Format.formatter_of_out_channel out in
  let print_sources fmt =
    fprintf fmt "[";
    writer.sources
    |> List.iteri (fun i source ->
      if i <> 0 then fprintf fmt ",";
      fprintf fmt "%S" (Uri.to_string source));
    fprintf fmt "]"
  in
  let print_sources_content fmt =
    fprintf fmt "[";
    writer.sources
    |> List.iteri (fun i source ->
      if i <> 0 then fprintf fmt ",";
      let source = Source.read source in
      fprintf fmt "%S" source.contents);
    fprintf fmt "]"
  in
  let print_names fmt =
    fprintf fmt "[";
    writer.names
    |> List.iteri (fun i name ->
      if i <> 0 then fprintf fmt ",";
      fprintf fmt "%S" name);
    fprintf fmt "]"
  in
  fprintf
    fmt
    "{\"version\":3,\"sources\":%t,\"sourcesContent\":%t,\"names\":%t,\"mappings\":%S}"
    print_sources
    print_sources_content
    print_names
    mappings;
  Format.pp_print_flush fmt ();
  close_out out;
  fprintf
    writer.fmt
    "\n//# sourceMappingURL=%s\n//"
    (Filename.basename writer.source_map_path)
;;

let write_string (s : string) (writer : t) : unit =
  fprintf writer.fmt "%s" s;
  s
  |> String.iter (fun c ->
    writer.pos <- writer.pos |> Position.advance c;
    if c = '\n'
    then (
      fprintf writer.mappings_fmt ";";
      writer.prev_mapping_column <- None))
;;

let write_newline (writer : t) : unit =
  writer |> write_string "\n";
  writer |> write_string writer.line_prefix
;;

let open_span (_span : span) (writer : t) : unit = writer.span_start <- Some writer.pos

let close_span (span : span) (writer : t) : unit =
  match writer.span_start with
  | None -> ()
  | Some start ->
    writer.span_start <- None;
    writer |> add_to_source_map start span.uri span.start ~name:None
;;

let write_name ~(original_name : string) ~(span : span) (name : string) (writer : t)
  : unit
  =
  writer |> add_to_source_map writer.pos span.uri span.start ~name:(Some original_name);
  writer |> write_string name;
  writer.span_start <- None
;;

let write_spanned (span : span) (f : unit -> unit) (writer : t) : unit =
  writer |> open_span span;
  f ();
  writer |> close_span span
;;
