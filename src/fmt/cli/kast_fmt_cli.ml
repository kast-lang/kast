open Std
open Util

module Args = struct
  type args = { path : path }
  type t = args

  let parse : string list -> args = function
    | [] -> { path = Stdin }
    | [ path ] -> { path = File path }
    | first :: _rest -> fail "unexpected arg %S" first
end

let run : Args.t -> unit =
 fun { path } ->
  let source = Source.read path in
  let ruleset = Default_syntax.ruleset in
  let parsed = Parser.parse source ruleset in
  parsed |> Kast_fmt.format Format.std_formatter;
  println ""
