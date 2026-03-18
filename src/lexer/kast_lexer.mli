open Std
open Kast_util
module Token = Kast_token

exception Error of (formatter -> unit)

module Reader = Reader

type lexer
type t = lexer
type rule = t -> Token.Shape.t option

val source : lexer -> source
val default_rules : rule list
val init : rule list -> source -> lexer
val init_with : rule list -> Token.t list -> Uri.t -> lexer
val position : lexer -> position
val peek : lexer -> Token.t
val next : lexer -> Token.t
val advance : lexer -> unit
val expect_next : lexer -> string -> unit
val expect_eof : lexer -> unit
val read_all : rule list -> source -> Token.t list

type recording

val start_rec : lexer -> recording
val stop_rec : recording -> Token.t list
val maybe_convert_to_raw_ident : string -> string
