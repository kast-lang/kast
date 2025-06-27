open Std
open Kast_util
module Lexer = Kast_lexer
module Parser = Kast_parser

module Args = struct
  type args = { path : path }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Stdin }
    | [ path ] -> { path = File path }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path } ->
  let source = Source.read path in
  let lexer = Lexer.init Lexer.default_rules source in
  let parsed = Parser.parse_with_lexer lexer Kast_default_syntax.ruleset in
  Kast_highlight.print Format.std_formatter parsed
