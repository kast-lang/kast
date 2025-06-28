open Format

module Option = struct
  include Stdlib.Option

  let map_or : 'a 'r. 'r -> ('a -> 'r) -> 'a option -> 'r =
   fun default f opt ->
    match opt with
    | Some x -> f x
    | None -> default

  let or_else : 'a. (unit -> 'a option) -> 'a option -> 'a option =
   fun f opt ->
    match opt with
    | Some x -> Some x
    | None -> f ()

  let unwrap_or_else : 'a. (unit -> 'a) -> 'a option -> 'a =
   fun f opt ->
    match opt with
    | Some x -> x
    | None -> f ()

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a option -> unit =
   fun print_value fmt opt ->
    match opt with
    | None -> fprintf fmt "<none>"
    | Some value -> print_value fmt value
end

let ( let* ) = Option.bind

(* unwrap_or_else *)
let ( let+ ) : 'a. 'a option -> (unit -> 'a) -> 'a =
 fun opt f ->
  match opt with
  | None -> f ()
  | Some x -> x

let ( let/ ) : 'a. 'a option -> ('a -> unit) -> unit =
 fun opt f ->
  match opt with
  | None -> ()
  | Some x -> f x
