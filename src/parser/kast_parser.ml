open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer
module Syntax = Kast_syntax
module Ast = Kast_ast.T
module Error = Error
module Parsed_part = Parsed_part
module Ruleset = Ruleset

type error = Error.t

let error = Error.error

type ruleset = Ruleset.t

type result =
  { ast : Ast.t
  ; trailing_comments : Token.comment list
  ; eof : position
  }

let parse_with_lexer : Lexer.t -> ruleset -> result =
  fun lexer ruleset ->
  let unused_comments_rev = ref [] in
  let result =
    Impl.parse_value
      { continuation_keywords = StringSet.empty
      ; filter = Any
      ; lexer
      ; ruleset
      ; unused_comments_rev
      }
    |> Option.get
  in
  Impl.expect_eof lexer;
  { ast = result
  ; trailing_comments = !unused_comments_rev |> List.rev
  ; eof = Lexer.position lexer
  }
;;

let parse : source -> ruleset -> result =
  fun source ruleset -> parse_with_lexer (Lexer.init Lexer.default_rules source) ruleset
;;
