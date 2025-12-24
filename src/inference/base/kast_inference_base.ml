open Std
open Kast_util
module Error = Error

type 'a unite = span:span -> 'a -> 'a -> 'a

module CompareRecurseCache = RecurseCache.Make (Id.Pair)

module Var = struct
  type 'a var = { mutable state : 'a var_state }

  and 'a var_state =
    | Root of { data : 'a var_data }
    | NotRoot of { closer_to_root : 'a var }

  and 'a var_data = {
    recurse_id : id;
    inferred : 'a option;
    mutable setup_default : (unit -> unit) option;
    spans : SpanSet.t;
    mutable once_inferred : ('a -> unit) list;
    mutable on_unite : ('a var_data -> unit) list;
    mutable subbed_in : Id.Set.t;
  }

  let new_not_inferred : 'a. span:span -> 'a var =
   fun ~span ->
    {
      state =
        Root
          {
            data =
              {
                recurse_id = Id.gen ();
                inferred = None;
                setup_default = None;
                once_inferred = [];
                on_unite = [];
                spans = SpanSet.singleton span;
                subbed_in = Id.Set.empty;
              };
          };
    }

  let new_inferred : 'a. span:span -> 'a -> 'a var =
   fun ~span inferred ->
    {
      state =
        Root
          {
            data =
              {
                recurse_id = Id.gen ();
                setup_default = None;
                inferred = Some inferred;
                once_inferred = [];
                on_unite = [];
                spans = SpanSet.singleton span;
                subbed_in = Id.Set.empty;
              };
          };
    }

  let rec find_root_var : 'a. 'a var -> 'a var =
   fun var ->
    match var.state with
    | Root _ -> var
    | NotRoot { closer_to_root } ->
        let root = find_root_var closer_to_root in
        var.state <- NotRoot { closer_to_root = root };
        root

  let find_root : 'a. 'a var -> 'a var_data =
   fun var ->
    let root_var = find_root_var var in
    match root_var.state with
    | Root { data } -> data
    | NotRoot _ -> unreachable "found non root when finding root"

  let inferred_opt : 'a. 'a var -> 'a option =
   fun var -> (find_root var).inferred

  let setup_default f var =
    let root_var = find_root_var var in
    let root = find_root root_var in
    root_var.state <- Root { data = { root with setup_default = Some f } }

  type 'a t = 'a var

  let run_once_inferred_if_needed : 'a. 'a var_data -> unit =
   fun data ->
    match data.inferred with
    | None -> ()
    | Some value ->
        let fs = data.once_inferred in
        data.once_inferred <- [];
        fs |> List.iter (fun f -> f value)

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit =
   fun print_inferred fmt var ->
    let {
      recurse_id;
      inferred;
      setup_default = _;
      once_inferred = _;
      on_unite = _;
      spans;
      subbed_in = _;
    } =
      find_root var
    in
    match inferred with
    | None -> fprintf fmt "_"
    (* fprintf fmt "_%a%a" Id.print recurse_id (List.print Span.print)
          (spans |> SpanSet.to_list) *)
    | Some inferred ->
        (* fprintf fmt "_%a%a=" Id.print recurse_id (List.print Span.print)
          (spans |> SpanSet.to_list); *)
        print_inferred fmt inferred

  let unite_data =
   fun ~span unite_inferred
       ({
          recurse_id = recurse_id_a;
          inferred = inferred_a;
          setup_default = default_a;
          once_inferred = _;
          on_unite = _;
          spans = spans_a;
          subbed_in = subbed_in_a;
        } as data_a)
       ({
          recurse_id = recurse_id_b;
          inferred = inferred_b;
          setup_default = default_b;
          once_inferred = _;
          on_unite = _;
          spans = spans_b;
          subbed_in = subbed_in_b;
        } as data_b) ->
    let inferred =
      match (inferred_a, inferred_b) with
      | None, None -> None
      | Some inferred, None | None, Some inferred -> Some inferred
      | Some a, Some b -> Some (unite_inferred ~span a b)
    in
    let once_inferred = data_a.once_inferred @ data_b.once_inferred in
    data_a.once_inferred <- [];
    data_b.once_inferred <- [];
    let on_unite = data_a.on_unite @ data_b.on_unite in
    data_a.on_unite <- [];
    data_b.on_unite <- [];
    Log.trace (fun log ->
        log "uniting vars %a and %a" Id.print recurse_id_a Id.print recurse_id_b);
    let data =
      {
        recurse_id = recurse_id_a;
        inferred;
        setup_default =
          (match (default_a, default_b) with
          | None, None -> None
          | Some x, None | None, Some x -> Some x
          | Some a, Some _b -> Some a);
        once_inferred;
        on_unite = [];
        spans = SpanSet.union spans_a spans_b;
        subbed_in = Id.Set.union subbed_in_a subbed_in_b;
      }
    in
    on_unite |> List.iter (fun f -> f data);
    data

  let remember_subbed : 'a. id -> 'a var -> unit =
   fun id var ->
    let root = find_root var in
    root.subbed_in <- root.subbed_in |> Id.Set.add id

  let remember_subbed_from : 'a. 'a var -> 'a var -> unit =
   fun from var ->
    let from = find_root from in
    let root = find_root var in
    root.subbed_in <- root.subbed_in |> Id.Set.union from.subbed_in;
    match (root.setup_default, from.setup_default) with
    | None, Some f ->
        let called = ref false in
        let f =
         fun () ->
          match !called with
          | false ->
              called := true;
              f ()
          | true -> ()
        in
        root.setup_default <- Some f;
        from.setup_default <- Some f
    | _ -> ()

  let was_subbed_in : 'a. Id.t -> 'a var -> bool =
   fun id var ->
    let root = find_root var in
    root.subbed_in |> Id.Set.mem id

  let unite : 'a. 'a unite -> 'a var unite =
   fun unite_inferred ~span a b ->
    try
      let root_a = find_root_var a in
      let root_b = find_root_var b in
      if root_a == root_b then a
      else
        let data_a = find_root a in
        let data_b = find_root b in
        let root_a, root_b =
          if Random.bool () then (root_a, root_b) else (root_b, root_a)
        in
        let temp_data : 'a var_data = data_a in
        root_a.state <- Root { data = temp_data };
        root_b.state <- NotRoot { closer_to_root = root_a };
        let united_data = unite_data ~span unite_inferred data_a data_b in
        let root = find_root_var a in
        let root_data = find_root a in
        let united_data_final =
          unite_data ~span unite_inferred united_data root_data
        in
        root.state <- Root { data = united_data_final };
        united_data_final |> run_once_inferred_if_needed;
        root
    with effect (Error.Error _ as eff), _k ->
      Effect.perform eff;
      a

  let infer_as : 'a. 'a unite -> span:span -> 'a -> 'a var -> unit =
   fun unite_inferred ~span infer_as var ->
    unite unite_inferred ~span (new_inferred ~span infer_as) var |> ignore

  let setup_default_if_needed var =
    match inferred_opt var with
    | Some _ -> ()
    | None -> (
        let root = find_root var in
        match root.setup_default with
        | None -> ()
        | Some f -> f ())

  let inferred_or_default =
   fun var ->
    setup_default_if_needed var;
    inferred_opt var

  let once_inferred : 'a. ('a -> unit) -> 'a var -> unit =
   fun f var ->
    let root_data = find_root var in
    root_data.once_inferred <- f :: root_data.once_inferred;
    root_data |> run_once_inferred_if_needed

  type _ Effect.t += AwaitUpdate : 'a. 'a var -> bool Effect.t

  let fork (f : unit -> unit) : unit =
    try f ()
    with effect AwaitUpdate var, k ->
      let k = dont_leak_please k in
      let f = fun _ -> k.continue true in
      once_inferred f var

  let rec await_inferred : 'a. error_shape:'a -> 'a var -> 'a =
   fun ~error_shape var ->
    let root_data = find_root var in
    match root_data.inferred with
    | Some inferred -> inferred
    | None ->
        if Effect.perform (AwaitUpdate var) then await_inferred ~error_shape var
        else error_shape

  let compare compare_inferred a b =
    let a = find_root a in
    let b = find_root b in
    if a.recurse_id = b.recurse_id then 0
    else
      match (a.inferred, b.inferred) with
      | Some a, Some b -> compare_inferred a b
      | Some _, None | None, Some _ | None, None ->
          Id.compare a.recurse_id b.recurse_id

  let equal equal_inferred a b =
    let a = find_root a in
    let b = find_root b in
    if a.recurse_id = b.recurse_id then true
    else
      let cache = CompareRecurseCache.get () in
      let ids = (a.recurse_id, b.recurse_id) in
      if cache |> CompareRecurseCache.is_visited ids then true
      else (
        cache |> CompareRecurseCache.enter ids;
        match (a.inferred, b.inferred) with
        | Some a, Some b -> equal_inferred a b
        | Some _, None | None, Some _ | None, None -> false)

  let recurse_id var =
    let data = find_root var in
    data.recurse_id

  let spans var =
    let data = find_root var in
    data.spans

  module Map = struct
    type 'a t = {
      id : Id.t;
      mutable by_recurse_id : (Id.t, 'a) Hashtbl.t;
    }

    let create () = { id = Id.gen (); by_recurse_id = Hashtbl.create 0 }

    let add var value map =
      let rec check_unite current_id data =
        let new_id = data.recurse_id in
        Hashtbl.remove map.by_recurse_id current_id;
        Log.trace (fun log ->
            log "Adding id %a, map size = %a" Id.print new_id Int.print
              (Hashtbl.length map.by_recurse_id));
        Hashtbl.add map.by_recurse_id new_id value;
        Log.trace (fun log ->
            log "varmap.add %a to map %a" Id.print new_id Id.print map.id);
        data.on_unite <- check_unite new_id :: data.on_unite
      in
      check_unite (recurse_id var) (find_root var)

    let find_opt var map =
      Log.trace (fun log ->
          log "varmap.find_opt %a in map %a" Id.print (recurse_id var) Id.print
            map.id);
      Hashtbl.find_opt map.by_recurse_id (recurse_id var)
  end
end

let fork = Var.fork

type 'a var = 'a Var.t

let equal_var = Var.equal
let compare_var = Var.compare
