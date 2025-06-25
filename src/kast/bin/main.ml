open Common
open Kast

let main () =
  let args = Cli.parse () in
  match args.command with
  | Cli.Command.Tokenize { path } ->
      let source = read path in
      let tokens = Lexer.read_all Lexer.default_rules source in
      tokens
      |> List.iter (fun token ->
             println "%a" (Spanned.print Lexer.Token.print) token)
  | Cli.Command.Parse { path } -> (
      let source = read path in
      let { ast; trailing_comments = _ } : Parser.result =
        Parser.parse source Default_syntax.ruleset
      in
      match ast with
      | Some ast -> println "%a" Ast.print ast
      | None -> println "<nothing>")
  | Cli.Command.Highlight args -> Highlight.perform args
  | Cli.Command.Lsp args -> Kast_lsp.run args
  | Cli.Command.Help ->
      println "Hello, I am Kast :)\nhelp is not implemented yet"
;;

try main () with
| Failure s ->
    eprintln "@{<red>%s@}" s;
    Printexc.print_backtrace stderr;
    exit 1
| Lexer.Error f ->
    Format.eprintf "@{<red>Lexer error@}: ";
    f Format.err_formatter;
    eprintln "";
    exit 1
| Parser.Error f ->
    Format.eprintf "@{<red>Parse error@}: ";
    f Format.err_formatter;
    eprintln "";
    exit 1
| FailFormat f ->
    Format.eprintf "@{<red>Error@}: ";
    f Format.err_formatter;
    eprintln "";
    exit 1
