open Std

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

module Path = struct
  type t =
    | File of string
    | Special of string
    | Stdin
  [@@deriving eq, ord]

  type path = t

  let as_file = function
    | File path -> Some path
    | _ -> None

  let parse = function
    | "-" -> Some Stdin
    | path -> Some (File path)

  let print fmt = function
    | File path -> fprintf fmt "%s" path
    | Special s -> fprintf fmt "<%s>" s
    | Stdin -> fprintf fmt "<stdin>"
end

module PathMap = Map.Make (Path)

type path = Path.t

module Source = struct
  type source = {
    contents : string;
    filename : path;
  }

  type t = source
  type _ Effect.t += ReadSpecial : string -> string Effect.t

  let read (path : path) : source =
    let contents =
      match path with
      | File path -> In_channel.input_all (In_channel.open_text path)
      | Special s -> Effect.perform (ReadSpecial s)
      | Stdin -> In_channel.input_all In_channel.stdin
    in
    { contents; filename = path }
end

type source = Source.t

module Span = struct
  type t = {
    start : position;
    finish : position;
    filename : path;
  }

  type span = t

  let fake s =
    {
      start = Position.beginning;
      finish = Position.beginning;
      filename = Special s;
    }

  let touches : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0
    && Position.compare pos span.finish <= 0

  let contains : position -> span -> bool =
   fun pos span ->
    Position.compare span.start pos <= 0 && Position.compare pos span.finish < 0

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; filename } ->
    fprintf fmt "%a:%d.%d-%d.%d" Path.print filename start.line start.column
      finish.line finish.column
end

type span = Span.t
