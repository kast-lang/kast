open Std
open Util;;

let args = Cli.parse () in
let read path : source =
  let channel =
    match path with
    | Cli.Path.File path -> In_channel.open_text path
    | Cli.Path.Stdin -> In_channel.stdin
  in
  let contents = In_channel.input_all channel in
  let filename =
    match path with
    | Cli.Path.File path -> path
    | Cli.Path.Stdin -> "<stdin>"
  in
  { contents; filename }
in
match args.command with
| Cli.Command.Tokenize { path } ->
    let source = read path in
    let tokens = Lexer.read_all Lexer.default_rules source in
    tokens
    |> List.iter (fun token ->
           println "%a" (Spanned.print Lexer.Token.print) token)
| Cli.Command.Parse { path } -> (
    let source = read path in
    let parsed = Parser.parse source Default_syntax.ruleset in
    match parsed with
    | Some ast -> println "@[<v>%a@]" Ast.print ast
    | None -> println "<nothing>")
| Cli.Command.Help -> println "Hello, I am Kast :)\nhelp is not implemented yet"
