open Std
open Kast_util
module Token = Kast_token
module Lexer = Kast_lexer

module Args = struct
  type args = { path : Uri.t }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Uri.stdin }
    | [ path ] -> { path = Uri.file path }
    | first :: _rest -> fail "Unexpected arg %a" String.print_debug first
  ;;
end

let run : Args.t -> unit =
  fun { path } ->
  let source = Source.read path in
  let tokens = Lexer.read_all Lexer.default_rules source in
  tokens |> List.iter (fun token -> println "%a" Token.print token)
;;
