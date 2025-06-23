open Option

module Set = struct
  include Stdlib.Set

  module Make (T : OrderedType) = struct
    include Stdlib.Set.Make (T)

    let contains : elt -> t -> bool =
     fun elem set -> find_opt elem set |> Option.is_some
  end
end
