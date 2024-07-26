type pos = { line : int; column : int }
type filename = Filename of string

let filename (Filename s) = s

type span = { start : pos; finish : pos; file : filename }
type 't spanned = { value : 't; span : span }

let show_pos pos =
  Int.to_string (pos.line + 1) ^ ":" ^ Int.to_string (pos.column + 1)

let show span =
  let (Filename file) = span.file in
  file ^ ":" ^ show_pos span.start ^ ".." ^ show_pos span.finish

let start_pos : pos = { line = 0; column = 0 }
