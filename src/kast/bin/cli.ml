open Std

module Command = struct
  type command =
    | Tokenize of Kast_lexer_cli.Args.t
    | Parse of Kast_parser_cli.Args.t
    | Highlight of Kast_highlight_cli.Args.t
    | Lsp of Kast_lsp_cli.Args.t
    | Fmt of Kast_fmt_cli.Args.t
    | Eval of Kast_interpreter_cli.Args.t
    | Run of Kast_interpreter_cli.Args.t
    | Repl of Kast_interpreter_cli.Args.t
    | Compile of Kast_compiler_cli.Args.t
    | Help

  type t = command

  let parse = function
    | [] -> Repl (Kast_interpreter_cli.Args.parse [])
    | ("lex" | "tokenize") :: args -> Tokenize (Kast_lexer_cli.Args.parse args)
    | "parse" :: args -> Parse (Kast_parser_cli.Args.parse args)
    | ("cat" | "highlight") :: args ->
        Highlight (Kast_highlight_cli.Args.parse args)
    | ("lsp" | "language-server") :: args -> Lsp (Kast_lsp_cli.Args.parse args)
    | ("fmt" | "format") :: args -> Fmt (Kast_fmt_cli.Args.parse args)
    | "eval" :: args -> Eval (Kast_interpreter_cli.Args.parse args)
    | "run" :: args -> Run (Kast_interpreter_cli.Args.parse args)
    | "compile" :: args -> Compile (Kast_compiler_cli.Args.parse args)
    | "repl" :: args -> Repl (Kast_interpreter_cli.Args.parse args)
    | args -> Run (Kast_interpreter_cli.Args.parse args)
end

type args = { command : Command.t }

let parse () : args =
  let args = Sys.argv |> Array.to_list |> List.tail in
  { command = Command.parse args }
