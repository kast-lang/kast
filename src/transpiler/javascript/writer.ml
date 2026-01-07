open Std
open Kast_util

type t = {
  fmt : formatter;
  source_map_path : string;
  mutable pos : position;
  mutable span_start : position option;
  mutable sources : Uri.t list;
  mutable source_idx : int UriMap.t;
  mappings_buffer : Buffer.t;
  mappings_fmt : formatter;
  mutable prev_source_idx : int option;
  mutable prev_mapping_column : int option;
  mutable prev_source_line : int option;
  mutable prev_source_column : int option;
}

let add_to_source_map (start : position) (_finish : position) (span : span)
    (writer : t) : unit =
  Log.info (fun log ->
      log "ADD TO SOURCE MAP %a %a" Position.print start Span.print span);
  let uri = span.uri in
  let source_idx =
    match writer.source_idx |> UriMap.find_opt uri with
    | Some idx -> idx
    | None ->
        let idx = writer.sources |> List.length in
        writer.sources <- List.append writer.sources [ uri ];
        writer.source_idx <- writer.source_idx |> UriMap.add uri idx;
        idx
  in
  if start.line = writer.pos.line then (
    let col =
      match writer.prev_mapping_column with
      | None -> start.column - 1
      | Some prev ->
          fprintf writer.mappings_fmt ",";
          start.column - 1 - prev
    in
    writer.prev_mapping_column <- Some (start.column - 1);
    Base64_vlq.print_base64_vlq writer.mappings_fmt col;
    (match writer.prev_source_idx with
    | None -> Base64_vlq.print_base64_vlq writer.mappings_fmt source_idx
    | Some prev ->
        Base64_vlq.print_base64_vlq writer.mappings_fmt (source_idx - prev));
    writer.prev_source_idx <- Some source_idx;
    (match writer.prev_source_line with
    | None ->
        Base64_vlq.print_base64_vlq writer.mappings_fmt (span.start.line - 1)
    | Some prev ->
        Base64_vlq.print_base64_vlq writer.mappings_fmt
          (span.start.line - 1 - prev));
    writer.prev_source_line <- Some (span.start.line - 1);
    (match writer.prev_source_column with
    | None ->
        Base64_vlq.print_base64_vlq writer.mappings_fmt (span.start.column - 1)
    | Some prev ->
        Base64_vlq.print_base64_vlq writer.mappings_fmt
          (span.start.column - 1 - prev));
    writer.prev_source_column <- Some (span.start.column - 1))

let init fmt source_map_path =
  let mappings_buffer = Buffer.create 32 in
  {
    fmt;
    source_map_path;
    pos = Position.beginning;
    span_start = None;
    sources = [];
    source_idx = UriMap.empty;
    mappings_buffer;
    mappings_fmt = Format.formatter_of_buffer mappings_buffer;
    prev_mapping_column = None;
    prev_source_idx = None;
    prev_source_line = None;
    prev_source_column = None;
  }

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
  fprintf fmt "{\"version\":3,\"sources\":%t,\"mappings\":%S}" print_sources
    mappings;
  Format.pp_print_flush fmt ();
  close_out out;
  fprintf writer.fmt "\n//# sourceMappingURL=%s\n//"
    (Filename.basename writer.source_map_path)

let write_string (s : string) (writer : t) : unit =
  fprintf writer.fmt "%s" s;
  s
  |> String.iter (fun c ->
      writer.pos <- writer.pos |> Position.advance c;
      if c = '\n' then (
        fprintf writer.mappings_fmt ";";
        writer.prev_mapping_column <- None))

let open_span (_span : span) (writer : t) : unit =
  writer.span_start <- Some writer.pos

let close_span (span : span) (writer : t) : unit =
  match writer.span_start with
  | None -> ()
  | Some start ->
      writer.span_start <- None;
      writer |> add_to_source_map start writer.pos span

let write_spanned (span : span) (f : unit -> unit) (writer : t) : unit =
  writer |> open_span span;
  f ();
  writer |> close_span span
