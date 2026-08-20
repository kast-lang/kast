open Std
open Kast_util

type reader =
  { contents : string
  ; mutable position : position
  }

type t = reader

let init : string -> reader = fun contents -> { contents; position = Position.beginning }

let prev : reader -> char option =
  fun reader -> String.get reader.contents (reader.position.index - 1)
;;

let peek : reader -> char option =
  fun reader -> String.get reader.contents reader.position.index
;;

let peek2 : reader -> char option =
  fun reader -> String.get reader.contents (reader.position.index + 1)
;;

let peek2_as_string : reader -> string =
  fun reader ->
  match peek reader with
  | None -> ""
  | Some c1 ->
    (match peek2 reader with
     | None -> String.make 1 c1
     | Some c2 -> make_string "%c%c" c1 c2)
;;

let peek_as_string : int -> reader -> string =
  fun n reader ->
  match n with
  | 1 ->
    (match peek reader with
     | None -> ""
     | Some c -> String.make 1 c)
  | 2 -> peek2_as_string reader
  | _ -> failwith __LOC__
;;

let advance : reader -> unit =
  fun reader ->
  match peek reader with
  | Some c -> reader.position <- Position.advance c reader.position
  | None -> failwith "tried to advance reader past eof"
;;

let skip : char -> reader -> unit =
  fun expected reader ->
  match peek reader with
  | Some c when c = expected -> advance reader
  | Some c ->
    failwith
    <| make_string "expected %C, got %C at %a" expected c Position.print reader.position
  | None -> failwith <| make_string "expected %C, got <eof>" expected
;;

type recording =
  { reader : reader
  ; start_index : int
  }

let start_rec : reader -> recording =
  fun reader -> { reader; start_index = reader.position.index }
;;

let finish_rec : recording -> string =
  fun { reader; start_index } ->
  let len = reader.position.index - start_index in
  if len = 0 then "" else String.sub reader.contents start_index len
;;

let read_while : (char -> bool) -> reader -> string =
  fun predicate reader ->
  let recording = start_rec reader in
  let rec loop () =
    match peek reader with
    | Some c when predicate c ->
      advance reader;
      loop ()
    | _ -> ()
  in
  loop ();
  finish_rec recording
;;
