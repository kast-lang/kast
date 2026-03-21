open Std
open Kast_util
module Parser = Kast_parser
module Ast = Kast_ast.T

module Args = struct
  type args =
    { path : Uri.t
    ; ruleset_path : Uri.t option
    }

  type t = args

  let rec parse : string list -> args = function
    | [] -> { path = Uri.stdin; ruleset_path = None }
    | [ path ] -> { path = Uri.file path; ruleset_path = None }
    | "--ruleset" :: path :: rest ->
      { (parse rest) with ruleset_path = Some (Uri.file path) }
    | first :: _rest -> fail "Unexpected arg %a" String.print_debug first
  ;;
end

let run : Args.t -> unit =
  fun { path; ruleset_path } ->
  let source = Source.read path in
  let { ast; trailing_comments = _; eof = _; ruleset_with_all_new_syntax = _ }
    : Parser.result
    =
    let ruleset =
      match ruleset_path with
      | None -> Kast_default_syntax.ruleset
      | Some path -> Kast_parser.Ruleset.parse_source (Source.read path)
    in
    Parser.parse source ruleset
  in
  println "%a" Ast.print ast
;;
