type pos = { line : int; column : int }
type filename = Filename of string
type span = { start : pos; finish : pos; file : filename }
type 't spanned = { value : 't; span : span }
