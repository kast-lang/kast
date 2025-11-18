open Format

module Id = struct
  module T = struct
    type t = { value : int }

    let next_id = ref 0

    let gen () =
      let value = !next_id in
      next_id := value + 1;
      { value }

    let compare a b = Int.compare a.value b.value
    let equal a b = Int.equal a.value b.value
    let hash id = Int.hash id.value

    let print : formatter -> t -> unit =
     fun fmt { value } -> fprintf fmt "#%d" value
  end

  include T
  module Map = Map.Make (T)
end

type id = Id.t
