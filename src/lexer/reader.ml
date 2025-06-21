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

type recording = { reader : reader; start_index : int }

let start_rec : reader -> recording =
 fun reader -> { reader; start_index = reader.position.index }

let finish_rec : recording -> string =
 fun { reader; start_index } ->
  String.sub reader.contents start_index (reader.position.index - start_index)

let read_while : (char -> bool) -> reader -> string =
 fun predicate reader ->
  let recording = start_rec reader in
  let rec loop () =
    match peek reader with
    | Some c when predicate c ->
        skip reader;
        loop ()
    | _ -> ()
  in
  loop ();
  finish_rec recording
