module Id = struct
  type t = { raw : int }

  let next_id = ref 0

  let gen () =
    next_id := !next_id + 1;
    { raw = !next_id }

  let compare a b = Int.compare a.raw b.raw
end

module Value = struct
  type t = {
    id : Id.t;
    mutable inferred : int option;
  }

  let compare a b =
    if a.id = b.id then 0
    else
      match (a.inferred, b.inferred) with
      | Some a, Some b -> Int.compare a b
      | _, _ -> Id.compare a.id b.id
end

module ValueMap = struct
  type 'a t = { entries : (Value.t * 'a) list }

  let empty = { entries = [] }
  let add key value map = { entries = (key, value) :: map.entries }

  let find key map =
    map.entries
    |> List.find_map (fun (existing_key, value) ->
        if Value.compare key existing_key = 0 then Some value else None)
end

let main () =
  let a : Value.t = { id = Id.gen (); inferred = None } in
  let b : Value.t = { id = Id.gen (); inferred = None } in
  let map = ValueMap.empty |> ValueMap.add a 0 |> ValueMap.add b 1 in
  map |> ValueMap.find b |> ignore;
  a.inferred <- Some 2;
  map |> ValueMap.find b |> ignore;
  b.inferred <- Some 1;
  map |> ValueMap.find b |> ignore

let () = main ()
