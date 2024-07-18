type t = Id of int64
type id = t

let next_id : int64 ref = ref @@ Int64.of_int 0

let gen () : id =
  let id = !next_id in
  next_id := Int64.add id (Int64.of_int 1);
  Id id

let show (Id id) = Int64.to_string id

module MapImpl = struct
  type t = id

  let compare (Id a) (Id b) = Int64.compare a b
end

module Map = Map.Make (MapImpl)
