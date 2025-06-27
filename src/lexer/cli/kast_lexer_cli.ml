open Std
open Util

module Args = struct
  type args = { path : path }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Stdin }
    | [ path ] -> { path = File path }
    | first :: _rest -> fail "Unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path } ->
  let source = Source.read path in
  let tokens = Lexer.read_all Lexer.default_rules source in
  tokens
  |> List.iter (fun token ->
         println "%a" (Spanned.print Lexer.Token.print) token)
