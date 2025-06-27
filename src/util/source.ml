open Std

module Position = struct
  type t = {
    index : int;
    line : int;
    column : int;
  }

  type position = t

  let beginning : position = { index = 0; line = 1; column = 1 }

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
  type path =
    | File of string
    | Special of string
    | Stdin

  type t = path

  let parse = function
    | "-" -> Some Stdin
    | path -> Some (File path)

  let print fmt = function
    | File path -> fprintf fmt "%s" path
    | Special s -> fprintf fmt "<%s>" s
    | Stdin -> fprintf fmt "<stdin>"
end

type path = Path.t

module Source = struct
  type source = {
    contents : string;
    filename : path;
  }

  type t = source

  let read (path : path) : source =
    let channel =
      match path with
      | File path -> In_channel.open_text path
      | Special s -> fail "No idea how to open %S" s
      | Stdin -> In_channel.stdin
    in
    let contents = In_channel.input_all channel in
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

  let print : 'a. formatter -> span -> unit =
   fun fmt { start; finish; filename } ->
    fprintf fmt "%a:%d.%d-%d.%d" Path.print filename start.line start.column
      finish.line finish.column
end

type span = Span.t

module Spanned = struct
  type 'a t = {
    value : 'a;
    span : span;
  }

  type 'a spanned = 'a t

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a spanned -> unit =
   fun print_value fmt spanned ->
    fprintf fmt "%a @{<dim>at %a@}" print_value spanned.value Span.print
      spanned.span
end

type 'a spanned = 'a Spanned.t
