open Std
open Kast

module Path = struct
  type path =
    | File of string
    | Stdin

  type t = path

  let parse = function
    | "-" -> Some Stdin
    | path -> Some (File path)
  (* | path when String.ends_with ~suffix:".ks" path -> Some (File path) *)
  (* | _ -> None *)
end

type path = Path.t

module Command = struct
  let unexpected arg = failwith @@ make_string "Unexpected arg %S" arg
  let is_valid_path path = String.ends_with ~suffix:".ks" path || path = "-"

  module Common = struct
    type t = { path : path }

    let parse_with_path path = function
      | [] -> { path }
      | arg :: _ -> unexpected arg

    let parse : string list -> t = function
      | [] -> { path = Stdin }
      | arg :: rest -> (
          match Path.parse arg with
          | Some path -> parse_with_path path rest
          | None -> unexpected arg)
  end

  type command =
    | Tokenize of Common.t
    | Parse of Common.t
    | Highlight of Common.t
    | Lsp of Kast_lsp.args
    | Help

  type t = command

  let parse = function
    | [] -> Help
    | "tokenize" :: args -> Tokenize (Common.parse args)
    | "parse" :: args -> Parse (Common.parse args)
    | "highlight" :: args -> Highlight (Common.parse args)
    | "lsp" :: args -> Lsp (Kast_lsp.parse args)
    | arg :: rest -> (
        match Path.parse arg with
        | Some path ->
            let common = Common.parse rest in
            ignore (path, common);
            failwith "not implemented"
        | None -> unexpected arg)
end

type args = { command : Command.t }

let parse () : args =
  let args = Sys.argv |> Array.to_list |> List.tail in
  { command = Command.parse args }
