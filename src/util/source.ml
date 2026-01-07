open Std
open String

module Position = struct
  type t = {
    index : int;
    line : int;
    column : int;
  }

  type position = t

  let beginning : position = { index = 0; line = 1; column = 1 }

  let compare : position -> position -> int =
   fun a b ->
    let line = Int.compare a.line b.line in
    if line <> 0 then line else Int.compare a.column b.column

  let equal : position -> position -> bool = fun a b -> compare a b = 0

  let advance : char -> position -> position =
   fun c p ->
    match c with
    | '\n' -> { index = p.index + 1; line = p.line + 1; column = 1 }
    | _ -> { index = p.index + 1; line = p.line; column = p.column + 1 }

  let print : 'a. formatter -> position -> unit =
   fun fmt { index = _; line; column } -> fprintf fmt "%d:%d" line column
end

type position = Position.t

module Uri = struct
  include Uri

  let stdin = of_string "stdin:"

  let file path =
    let cwd = make ~scheme:"file" ~host:"" ~path:(Sys.getcwd () ^ "/") () in
    let path = Uri.of_string path in
    Uri.resolve "" cwd path

  let print_full : formatter -> Uri.t -> unit =
   fun fmt uri -> fprintf fmt "%s" (Uri.to_string uri)

  let print : formatter -> Uri.t -> unit =
   fun fmt uri ->
    if scheme uri = Some "file" then fprintf fmt "%s" (Uri.path uri)
    else print_full fmt uri

  let append_if_relative parent maybe_relative =
    match Uri.scheme maybe_relative with
    | None -> (
        let path = Uri.path maybe_relative in
        let relative =
          path
          |> String.strip_prefix ~prefix:"./"
          |> Option.or_else (fun () : string option ->
              if String.starts_with ~prefix:"/" path then None else Some path)
        in
        match relative with
        | Some relative ->
            Uri.with_path parent (Uri.path parent ^ "/" ^ relative)
        | None -> maybe_relative)
    | _ -> maybe_relative

  let maybe_relative_to_file file maybe_relative =
    match Uri.scheme maybe_relative with
    | None -> (
        let path = Uri.path maybe_relative in
        let relative =
          path
          |> String.strip_prefix ~prefix:"./"
          |> Option.or_else (fun () : string option ->
              if String.starts_with ~prefix:"/" path then None else Some path)
        in
        match relative with
        | Some _relative ->
            let file_path = Uri.path file in
            let parent =
              String.sub file_path 0 (String.rindex file_path '/' + 1)
            in
            let parent_uri = Uri.with_path file parent in
            Uri.resolve "" parent_uri maybe_relative
        | None -> maybe_relative)
    | _ -> maybe_relative
end

module UriMap = Map.Make (Uri)

module Source = struct
  type source = {
    contents : string;
    uri : Uri.t;
  }

  type t = source
  type _ Effect.t += Read : Uri.t -> string Effect.t

  let read (uri : Uri.t) : source =
    let contents = Effect.perform (Read uri) in
    { contents; uri }
end

type source = Source.t

module Span = struct
  type t = {
    start : Position.t;
    finish : Position.t;
    uri : Uri.t;
  }
  [@@deriving eq, ord]

  type span = t

  let length span = span.finish.index - span.start.index

  let compare a b =
    let c = Position.compare a.start b.start in
    if c <> 0 then c
    else
      let c = Position.compare a.finish b.finish in
      if c <> 0 then c else Uri.compare a.uri b.uri

  let beginning_of uri =
    { start = Position.beginning; finish = Position.beginning; uri }

  let single_char pos uri = { start = pos; finish = pos; uri }
  let fake desc = beginning_of <| Uri.of_string ("fake:" ^ desc)

  let of_ocaml : string * int * int * int -> span =
   fun (file, line, col, _enum) ->
    {
      uri = Uri.of_string file;
      start = { index = 0; line; column = col };
      finish = { index = 0; line; column = col };
    }

  let touches : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0
    && Position.compare pos span.finish <= 0

  let contains_position : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0 && Position.compare pos span.finish < 0

  let contains_span : span -> span -> bool =
   fun inner outer ->
    Uri.equal inner.uri outer.uri
    && Position.compare outer.start inner.start <= 0
    && Position.compare inner.finish outer.finish <= 0

  let print_with : 'a. (formatter -> Uri.t -> unit) -> formatter -> span -> unit
      =
   fun print_uri fmt { start; finish; uri } ->
    fprintf fmt "%a:%d.%d-%d.%d" print_uri uri start.line start.column
      finish.line finish.column

  let print = print_with Uri.print

  let print_osc8 :
      'a. span -> (formatter -> 'a -> unit) -> 'a -> formatter -> unit =
   fun span print_value value fmt ->
    let print_span fmt { start; finish; uri } =
      fprintf fmt "vscode://file%a:%d:%d" Uri.print uri start.line start.column
      (* fprintf fmt "vscode://file%a:%d.%d-%d.%d" Uri.print uri start.line
        start.column finish.line finish.column *)
    in
    let printed_span = make_string "%a" print_span span in
    Format.pp_open_stag fmt (Format.OSC8 printed_span);
    print_value fmt value;
    Format.pp_close_stag fmt ()
end

module SpanSet = Set.Make (Span)

type span = Span.t
