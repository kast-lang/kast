open Id

module RecurseCache = struct
  type recurse_cache = { visited : (id, int) Hashtbl.t }
  type t = recurse_cache
  type _ Effect.t += Get : unit -> recurse_cache Effect.t

  let get () = Effect.perform (Get ())

  let with_cache (cache : recurse_cache) f =
    try f () with effect Get (), k -> Effect.Deep.continue k cache

  let create () = { visited = Hashtbl.create 0 }

  let visit_count (id : id) { visited } : int =
    Hashtbl.find_opt visited id |> Option.value ~default:0

  let is_visited (id : id) { visited } : bool =
    Hashtbl.find_opt visited id |> Option.is_some

  let visit (id : id) { visited } : bool =
    let visit_count = { visited } |> visit_count id in
    Hashtbl.add visited id (visit_count + 1);
    visit_count = 0
end
