include Format
include Char
include String
include Option
include List
include Array
include Return
include Ord
include Range
include Log
include Bool
include Set
include Int
include Effect
include Seq

exception Cancel

type ('a, 'b) unleakable_continuation = { continue : 'a -> 'b }

let dont_leak_please : ('a, 'b) continuation -> ('a, 'b) unleakable_continuation =
  fun k ->
  let resumed = ref false in
  let uk =
    { continue =
        (fun x ->
          if !resumed then failwith "CONTINUATION RESUMED TWICE BRO";
          resumed := true;
          Effect.Deep.continue k x)
    }
  in
  (* if false then *)
  Gc.finalise
    (fun _ ->
       if not !resumed
       then (
         resumed := true;
         (* CANT Log.error here, leads to segfaults :) *)
         (* Log.error (fun log -> log "leaked continuation");  *)
         try Effect.Deep.discontinue k Cancel with
         | _ -> ()))
    uk.continue;
  uk
;;

let unreachable format =
  Format.fprintf Format.str_formatter "unreachable reached: ";
  Format.kfprintf
    (fun _fmt ->
       let msg = Format.flush_str_formatter () in
       failwith msg)
    Format.str_formatter
    format
;;

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

exception FailFormat of (formatter -> unit)

let () =
  Printexc.register_printer (function
    | FailFormat f ->
      f Format.str_formatter;
      Some (Format.flush_str_formatter ())
    | _ -> None)
;;

let ( <| ) = ( @@ )
let fail f = Format.kdprintf (fun f -> raise <| FailFormat f) f

let rec create_dir_all path =
  if path = "" || path = "."
  then ()
  else (
    create_dir_all (Filename.dirname path);
    try Unix.mkdir path 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
;;

let read_from_filesystem path =
  let ch = In_channel.open_text path in
  Fun.protect (fun () -> In_channel.input_all ch) ~finally:(fun () -> In_channel.close ch)
;;

let timed name f =
  let start = Sys.time () in
  let result = f () in
  let time = Sys.time () -. start in
  Log.info (fun log -> log "time for %S = %f s" name time);
  result
;;
