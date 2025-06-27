open Std
open Kast_util
module Token = Kast_token

exception Error of (formatter -> unit)

module Reader = Reader

type rule = Reader.t -> Token.t option
type lexer
type t = lexer

val source : lexer -> source
val default_rules : rule list
val init : rule list -> source -> lexer
val position : lexer -> position
val peek : lexer -> Token.t spanned
val next : lexer -> Token.t spanned
val advance : lexer -> unit
val expect_next : lexer -> string -> unit
val expect_eof : lexer -> unit
val read_all : rule list -> source -> Token.t spanned list
