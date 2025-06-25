open Std
open Util

type args = { path : string option }

let parse : string list -> args = function
  | [] -> { path = None }
  | arg :: _rest -> fail "Unexpected arg %S" arg

module IoMonad = struct
  type 'a t = 'a

  let return : 'a. 'a -> 'a t = fun x -> x
  let raise : 'a. exn -> 'a t = fun e -> raise e

  module O = struct
    let ( let+ ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t = fun value f -> f value
    let ( let* ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun value f -> f value
  end
end

module Channel = struct
  type input = in_channel
  type output = out_channel

  let read_line : input -> string option IoMonad.t = In_channel.input_line

  let read_exactly : input -> int -> string option IoMonad.t =
    In_channel.really_input_string

  let write : output -> string list -> unit IoMonad.t =
   fun out list -> list |> List.iter (Out_channel.output_string out)
end

module Io = Lsp.Io.Make (IoMonad) (Channel)

let run : args -> unit =
 fun _ ->
  let rec loop () =
    match Io.read stdin with
    | Some packet ->
        let json : Jsonrpc.Json.t = Jsonrpc.Packet.yojson_of_t packet in
        let json_string =
          match json with
          | `Assoc _ -> "Assoc"
          | `Bool _ -> "Bool"
          | `Float _ -> "Float"
          | `Int _ -> "Int"
          | `Intlit _ -> "Intlit"
          | `List _ -> "List"
          | `Null -> "Null"
          | `String _ -> "String"
          | `Tuple _ -> "Tuple"
          | `Variant _ -> "Variant"
        in
        eprintln "%S" json_string;
        loop ()
    | None -> ()
  in
  loop ()
