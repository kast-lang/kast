open Id

module RecurseCache = struct
  type recurse_cache = { visited : (id, unit) Hashtbl.t }
  type t = recurse_cache

  let create () = { visited = Hashtbl.create 0 }

  let is_visited (id : id) { visited } : bool =
    Hashtbl.find_opt visited id |> Option.is_some

  let visit (id : id) { visited } : bool =
    let visited_first_time = not (is_visited id { visited }) in
    Hashtbl.add visited id ();
    visited_first_time
end
