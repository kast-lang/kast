open Std
open Kast_util

exception Error of (formatter -> unit)

module Token : sig
  type t = Token.t
  type token = t

  val print : formatter -> token -> unit
  val raw : token -> string option
  val is_raw : string -> token -> bool
  val as_float : token -> float

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
val position : lexer -> position
val peek : lexer -> token spanned
val next : lexer -> token spanned
val advance : lexer -> unit
val expect_next : lexer -> string -> unit
val expect_eof : lexer -> unit
val read_all : rule list -> source -> token spanned list
