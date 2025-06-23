open Format
open List

module Array = struct
  include Stdlib.Array

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a array -> unit =
   fun print_value fmt array -> array |> to_list |> List.print print_value fmt
end
