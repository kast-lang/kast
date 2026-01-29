open Std
module Parser = Kast_parser

let ruleset : Parser.ruleset =
  try
    Parser.Ruleset.parse_source
      { contents = [%include_file "rules.ks"]
      ; uri = Uri.of_string (Filename.dirname __FILE__ ^ "/rules.ks")
      }
  with
  | effect Parser.Error.Error e, k ->
    Effect.Deep.discontinue k (Failure (make_string "%a" Parser.Error.print e))
;;
