open Format
open List

module Array = struct
  include Stdlib.Array

  let get_opt : 'a. 'a t -> int -> 'a option =
   fun a idx -> if idx < 0 || idx >= length a then None else Some (get a idx)

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a array -> unit =
   fun print_value fmt array -> array |> to_list |> List.print print_value fmt
end
