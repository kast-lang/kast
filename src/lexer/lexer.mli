open Util
module Token = Token
module Reader = Reader

type token = Token.t
type rule = Reader.t -> token option
type lexer

val default_rules : rule list
val init : rule list -> source -> lexer
val peek : lexer -> token spanned
val next : lexer -> token spanned
val skip : lexer -> unit
val expect_next : lexer -> string -> unit
val read_all : rule list -> source -> token spanned list
