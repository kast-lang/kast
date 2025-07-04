open Std
open Kast_util

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
  with
  | effect Kast_compiler.Effect.FileIncluded _, k -> Effect.Deep.continue k ()
  | effect Kast_compiler.Effect.FindStd, k ->
      Effect.Deep.continue k (Uri.file "std")
  | effect Source.Read uri, k ->
      Effect.continue_with k (fun () ->
          match Uri.scheme uri with
          | Some "file" ->
              let path = Uri.path uri in
              read_from_filesystem path
          | Some "stdin" -> In_channel.input_all stdin
          | scheme ->
              fail "unsupported uri scheme %a"
                (Option.print String.print_dbg)
                scheme)

let () = main ()
