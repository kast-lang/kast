open Std
open Kast_util
open Kast_highlight
module Lexer = Kast_lexer
module Parser = Kast_parser

module Args = struct
  type args = {
    path : path;
    output : output;
  }

  type t = args

  let parse : string list -> args = function
    (* stdin *)
    | [] -> { path = Stdin; output = Term }
    | [ "--html" ] -> { path = Stdin; output = Html }
    | [ ("--term" | "--terminal") ] -> { path = Stdin; output = Term }
    (* file *)
    | [ path ] -> { path = File path; output = Term }
    | [ "--html"; path ] | [ path; "--html" ] ->
        { path = File path; output = Html }
    | [ ("--term" | "--terminal"); path ] | [ path; ("--term" | "--terminal") ]
      ->
        { path = File path; output = Term }
    | _ :: first :: _rest ->
        fail "Unexpected arg %S, expecting --html or --term" first
end

let run : Args.t -> unit =
 fun { path; output } ->
  let source = Source.read path in
  let lexer = Lexer.init Lexer.default_rules source in
  let parsed = Parser.parse_with_lexer lexer Kast_default_syntax.ruleset in
  let print =
    match output with
    | Term -> (module Term : Output)
    | Html -> (module Html : Output)
  in
  Kast_highlight.print print Format.std_formatter parsed
