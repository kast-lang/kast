open Std

let () =
  let args = Cli.parse () in
  match args.command with
  | Cli.Command.Tokenize args -> Kast_lexer_cli.run args
  | Cli.Command.Parse args -> Kast_parser_cli.run args
  | Cli.Command.Highlight args -> Kast_highlight_cli.run args
  | Cli.Command.Lsp args -> Kast_lsp_cli.run args
  | Cli.Command.Fmt args -> Kast_fmt_cli.run args
  | Cli.Command.Eval args -> Kast_interpreter_cli.eval args
  | Cli.Command.Help ->
      println "Hello, I am Kast :)\nhelp is not implemented yet"
