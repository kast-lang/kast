open Std

let main () =
  try
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
    | Cli.Command.Help ->
        println "Hello, I am Kast :)\nhelp is not implemented yet"
  with effect Kast_compiler.Effect.FileIncluded _, k ->
    Effect.Deep.continue k ()

let () = Kast_special_files_detached.with_special_files main
