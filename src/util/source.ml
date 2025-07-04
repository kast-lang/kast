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
  let file path = make ~scheme:"file" ~path ()

  let print : formatter -> Uri.t -> unit =
   fun fmt uri ->
    if scheme uri = Some "file" then fprintf fmt "%s" (Uri.path uri)
    else fprintf fmt "%s" (Uri.to_string uri)

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
        | Some relative ->
            let file_path = Uri.path file in
            let parent =
              String.sub file_path 0 (String.rindex file_path '/' + 1)
            in
            Uri.with_path file (parent ^ relative)
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
    start : position;
    finish : position;
    uri : Uri.t;
  }

  type span = t

  let beginning_of uri =
    { start = Position.beginning; finish = Position.beginning; uri }

  let fake desc = beginning_of <| Uri.of_string ("fake:" ^ desc)

  let touches : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0
    && Position.compare pos span.finish <= 0

  let contains : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0 && Position.compare pos span.finish < 0

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; uri } ->
    fprintf fmt "%a:%d.%d-%d.%d" Uri.print uri start.line start.column
      finish.line finish.column
end

type span = Span.t
