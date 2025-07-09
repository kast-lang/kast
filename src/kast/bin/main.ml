open Std
open Kast_util

let main () =
  (* Log.set_max_level Trace; *)
  let args = Cli.parse () in
  match args.command with
  | Cli.Command.Tokenize args -> Kast_lexer_cli.run args
  | Cli.Command.Parse args -> Kast_parser_cli.run args
  | Cli.Command.Highlight args -> Kast_highlight_cli.run args
  | Cli.Command.Lsp args -> Kast_lsp_cli.run args
  | Cli.Command.Fmt args -> Kast_fmt_cli.run args
  | Cli.Command.Eval args -> Kast_interpreter_cli.eval args
  | Cli.Command.Run args -> Kast_interpreter_cli.run args
  | Cli.Command.Compile args -> Kast_compiler_cli.run args
  | Cli.Command.Repl args -> Kast_interpreter_cli.repl args
  | Cli.Command.Help ->
      println "Hello, I am Kast :)\nhelp is not implemented yet"

let () = Kast.handle_effects main
