open Std
open Util

module Token : sig
  type t = Token.t
  type token = t

  val print : formatter -> token -> unit
  val raw : token -> string option
  val is_raw : string -> token -> bool

  type punct = Token.punct
  type ident = Token.ident
  type string = Token.string
  type number = Token.number
  type comment = Token.comment
end

module Reader = Reader

type token = Token.t
type rule = Reader.t -> token option
type lexer
type t = lexer

val source : lexer -> source
val default_rules : rule list
val init : rule list -> source -> lexer
val peek : lexer -> token spanned
val next : lexer -> token spanned
val skip : lexer -> unit
val skip_comments : lexer -> unit
val expect_next : lexer -> string -> unit
val read_all : rule list -> source -> token spanned list
