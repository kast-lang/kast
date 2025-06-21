open Stdext
open Util

type reader = { contents : string; mutable position : position }
type t = reader

let init : string -> reader =
 fun contents -> { contents; position = Position.beginning }

let peek : reader -> char option =
 fun reader -> String.get reader.contents reader.position.index

let peek2 : reader -> char option =
 fun reader -> String.get reader.contents (reader.position.index + 1)

let skip : reader -> unit =
 fun reader ->
  match peek reader with
  | None -> failwith "huh"
  | Some c -> reader.position <- Position.advance c reader.position

let read_while : (char -> bool) -> reader -> string =
 fun predicate reader ->
  let buffer = Buffer.create 0 in
  let rec loop () =
    match peek reader with
    | Some c when predicate c ->
        skip reader;
        Buffer.add_char buffer c;
        loop ()
    | _ -> ()
  in
  loop ();
  Buffer.contents buffer
