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
    match path with Cli.Path.File path -> path | Cli.Path.Stdin -> "<stdin>"
  in
  { contents; filename }
in
match args.command with
| Cli.Command.Tokenize { path } ->
    let source = read path in
    let tokens = Lexer.read_all Lexer.default_rules source in
    println "parsed tokens: %a"
      (Lexer.Token.print |> Spanned.print |> List.print)
      tokens
| Cli.Command.Parse { path } ->
    let source = read path in
    failwith "todo"
    (* Parser.parse source *)
| Cli.Command.Help -> println "Hello, I am Kast :)\nhelp is not implemented yet"
