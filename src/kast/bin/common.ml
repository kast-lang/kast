include Std
include Util

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
