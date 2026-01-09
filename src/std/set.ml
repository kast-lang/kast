open Option
open Format
open List

module Set = struct
  include Stdlib.Set

  module Make (T : OrderedType) = struct
    include Stdlib.Set.Make (T)

    let contains : elt -> t -> bool = fun elem set -> find_opt elem set |> Option.is_some

    let print : (formatter -> elt -> unit) -> formatter -> t -> unit =
      fun print_elem fmt set -> List.print print_elem fmt (to_list set)
    ;;
  end
end
