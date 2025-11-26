open Id

module RecurseCache = struct
  type recurse_cache = {
    visit_count : (id, int) Hashtbl.t;
    depth : (id, int) Hashtbl.t;
  }

  type t = recurse_cache
  type _ Effect.t += Get : unit -> recurse_cache Effect.t

  let get () = Effect.perform (Get ())

  let with_cache (cache : recurse_cache) f =
    try f () with effect Get (), k -> Effect.Deep.continue k cache

  let create () = { visit_count = Hashtbl.create 0; depth = Hashtbl.create 0 }

  let visit_count (id : id) cache : int =
    Hashtbl.find_opt cache.visit_count id |> Option.value ~default:0

  let is_visited (id : id) cache : bool =
    Hashtbl.find_opt cache.visit_count id |> Option.is_some

  let depth (id : id) cache : int =
    Hashtbl.find_opt cache.depth id |> Option.value ~default:0

  let enter (id : id) cache : unit =
    let visit_count = cache |> visit_count id in
    Hashtbl.add cache.visit_count id (visit_count + 1);
    let current_depth = cache |> depth id in
    Hashtbl.add cache.depth id (current_depth + 1)

  let exit (id : id) cache =
    let current_depth = cache |> depth id in
    Hashtbl.add cache.depth id (current_depth - 1)

  let clear { visit_count; depth } =
    Hashtbl.clear visit_count;
    Hashtbl.clear depth
end
