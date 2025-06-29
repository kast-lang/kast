open Std
open Kast_util
open Kast_highlight
module Parser = Kast_parser

module Args = struct
  type args = {
    path : path;
    hl_output : Kast_highlight.output option;
  }

  type t = args

  let parse : string list -> args = function
    (* stdin, no --highlight specified *)
    | [] -> { path = Stdin; hl_output = Some Term }
    (* file, no --highlight specified *)
    | [ path ] -> { path = File path; hl_output = Some Term }
    (* stdin, --highlight specified *)
    | [ "--highlight"; "none" ] -> { path = Stdin; hl_output = None }
    | [ "--highlight"; "html" ] -> { path = Stdin; hl_output = Some Html }
    | [ "--highlight"; ("term" | "terminal") ] ->
        { path = Stdin; hl_output = Some Term }
    | [ "--highlight"; output ] ->
        fail
          "Unexpected highlight output '%S', only 'html', 'term' or 'none' are allowed"
          output
    (* file, --highlight specified *)
    | [ "--highlight"; "none"; path ] | [ path; "--highlight"; "none" ] ->
        { path = File path; hl_output = None }
    | [ "--highlight"; "html"; path ] | [ path; "--highlight"; "html" ] ->
        { path = File path; hl_output = Some Html }
    | [ "--highlight"; ("term" | "terminal"); path ]
    | [ path; "--highlight"; ("term" | "terminal") ] ->
        { path = File path; hl_output = Some Term }
    | [ "--highlight"; output; _ ] ->
        fail
          "Unexpected highlight output '%S', only 'html', 'term' or 'none' are allowed"
          output
    | first :: _rest ->
        fail "Unexpected arg %S, expecting filename or option --highlight" first
end

let run : Args.t -> unit =
 fun { path; hl_output } ->
  let source = Source.read path in
  let ruleset = Kast_default_syntax.ruleset in
  let parsed = Parser.parse source ruleset in
  parsed |> Kast_fmt.format Format.str_formatter;
  let formatted = Format.flush_str_formatter () in
  match hl_output with
  | Some output ->
      let parsed =
        Parser.parse
          { contents = formatted; filename = Special "formatted" }
          ruleset
      in
      let highlight_print =
        match output with
        | Term -> (module Term : Output)
        | Html -> (module Html : Output)
      in
      Kast_highlight.print highlight_print Format.std_formatter parsed
  | None -> Format.printf "%s" formatted
