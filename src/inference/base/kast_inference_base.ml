open Std
open Kast_util
module Error = Error

type 'a unite = span:span -> 'a -> 'a -> 'a

module CompareRecurseCache = RecurseCache.Make (Id.Pair)

module type Scope = sig
  type t

  val root : unit -> t
  val unite : t -> t -> t
  val deepest : t -> t -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Var = struct
  type ('a, 'scope) var = { mutable state : ('a, 'scope) var_state }

  and ('a, 'scope) var_state =
    | Root of { data : ('a, 'scope) var_data }
    | NotRoot of { closer_to_root : ('a, 'scope) var }

  and ('a, 'scope) var_data =
    { recurse_id : id
    ; inferred : 'a option
    ; mutable setup_default : (unit -> unit) option
    ; spans : SpanSet.t
    ; mutable once_inferred : ('a -> unit) list
    ; mutable on_unite : (('a, 'scope) var_data -> unit) list
    ; mutable scope : 'scope
    }

  let new_not_inferred : 'a 'scope. span:span -> scope:'scope -> ('a, 'scope) var =
    fun ~span ~scope ->
    { state =
        Root
          { data =
              { recurse_id = Id.gen ()
              ; inferred = None
              ; setup_default = None
              ; once_inferred = []
              ; on_unite = []
              ; spans = SpanSet.singleton span
              ; scope
              }
          }
    }
  ;;

  let new_inferred : 'a 'scope. ('a -> 'scope) -> span:span -> 'a -> ('a, 'scope) var =
    fun inferred_scope ~span inferred ->
    { state =
        Root
          { data =
              { recurse_id = Id.gen ()
              ; setup_default = None
              ; inferred = Some inferred
              ; once_inferred = []
              ; on_unite = []
              ; spans = SpanSet.singleton span
              ; scope = inferred_scope inferred
              }
          }
    }
  ;;

  let rec find_root_var : 'a 'scope. _ var -> _ var =
    fun var ->
    match var.state with
    | Root _ -> var
    | NotRoot { closer_to_root } ->
      let root = find_root_var closer_to_root in
      var.state <- NotRoot { closer_to_root = root };
      root
  ;;

  let find_root : 'a 'scope. ('a, 'scope) var -> ('a, 'scope) var_data =
    fun var ->
    let root_var = find_root_var var in
    match root_var.state with
    | Root { data } -> data
    | NotRoot _ -> unreachable "found non root when finding root"
  ;;

  let inferred_opt : 'a 'scope. ('a, 'scope) var -> 'a option =
    fun var -> (find_root var).inferred
  ;;

  let setup_default f var =
    let root_var = find_root_var var in
    let root = find_root root_var in
    root_var.state <- Root { data = { root with setup_default = Some f } }
  ;;

  type ('a, 'scope) t = ('a, 'scope) var

  let run_once_inferred_if_needed : 'a 'scope. ('a, 'scope) var_data -> unit =
    fun data ->
    match data.inferred with
    | None -> ()
    | Some value ->
      let fs = data.once_inferred in
      data.once_inferred <- [];
      fs |> List.iter (fun f -> f value)
  ;;

  let print
    : 'a 'scope. (formatter -> 'a -> unit) -> formatter -> ('a, 'scope) var -> unit
    =
    fun print_inferred fmt var ->
    let { recurse_id
        ; inferred
        ; setup_default = _
        ; once_inferred = _
        ; on_unite = _
        ; spans
        ; scope = _
        }
      =
      find_root var
    in
    if false
    then
      fprintf
        fmt
        "%a%a="
        Id.print
        recurse_id
        (List.print Span.print)
        (spans |> SpanSet.to_list);
    match inferred with
    | None -> fprintf fmt "_"
    | Some inferred -> print_inferred fmt inferred
  ;;

  let unite_data =
    fun ~span
      unite_inferred
      unite_scope
      ({ recurse_id = recurse_id_a
       ; inferred = inferred_a
       ; setup_default = default_a
       ; once_inferred = _
       ; on_unite = _
       ; spans = spans_a
       ; scope = scope_a
       } as data_a)
      ({ recurse_id = recurse_id_b
       ; inferred = inferred_b
       ; setup_default = default_b
       ; once_inferred = _
       ; on_unite = _
       ; spans = spans_b
       ; scope = scope_b
       } as data_b) ->
    let inferred =
      match inferred_a, inferred_b with
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
      { recurse_id = recurse_id_a
      ; inferred
      ; setup_default =
          (match default_a, default_b with
           | None, None -> None
           | Some x, None | None, Some x -> Some x
           | Some a, Some _b -> Some a)
      ; once_inferred
      ; on_unite = []
      ; spans = SpanSet.union spans_a spans_b
      ; scope = unite_scope scope_a scope_b
      }
    in
    on_unite |> List.iter (fun f -> f data);
    data
  ;;

  let scope : 'a 'scope. ('a, 'scope) var -> 'scope =
    fun var ->
    let root = find_root var in
    root.scope
  ;;

  let unite
    : 'a 'scope. 'a unite -> ('scope -> 'scope -> 'scope) -> ('a, 'scope) var unite
    =
    fun unite_inferred unite_scope ~span a b ->
    let root_a = find_root_var a in
    let root_b = find_root_var b in
    if root_a == root_b
    then a
    else (
      let data_a = find_root a in
      let data_b = find_root b in
      let root_a, root_b = if Random.bool () then root_a, root_b else root_b, root_a in
      let temp_data : ('a, 'scope) var_data = data_a in
      let old_state_a = root_a.state in
      let old_state_b = root_b.state in
      root_a.state <- Root { data = temp_data };
      root_b.state <- NotRoot { closer_to_root = root_a };
      try
        let united_data = unite_data ~span unite_inferred unite_scope data_a data_b in
        let root = find_root_var a in
        let root_data = find_root a in
        let united_data_final =
          unite_data ~span unite_inferred unite_scope united_data root_data
        in
        root.state <- Root { data = united_data_final };
        united_data_final |> run_once_inferred_if_needed;
        root
      with
      | effect (Error.Error _ as eff), _k ->
        (* TODO this is probably very wrong *)
        root_a.state <- old_state_a;
        root_b.state <- old_state_b;
        Effect.perform eff;
        a)
  ;;

  let infer_as
    :  'a 'scope.
       ('a -> 'scope)
    -> 'a unite
    -> ('scope -> 'scope -> 'scope)
    -> span:span
    -> 'a
    -> ('a, 'scope) var
    -> unit
    =
    fun inferred_scope unite_inferred unite_scope ~span infer_as var ->
    unite
      unite_inferred
      unite_scope
      ~span
      (new_inferred inferred_scope ~span infer_as)
      var
    |> ignore
  ;;

  let setup_default_if_needed var =
    match inferred_opt var with
    | Some _ -> ()
    | None ->
      let root = find_root var in
      (match root.setup_default with
       | None -> ()
       | Some f -> f ())
  ;;

  let inferred_or_default =
    fun var ->
    setup_default_if_needed var;
    inferred_opt var
  ;;

  let once_inferred : 'a 'scope. ('a -> unit) -> ('a, 'scope) var -> unit =
    fun f var ->
    let root_data = find_root var in
    root_data.once_inferred <- f :: root_data.once_inferred;
    root_data |> run_once_inferred_if_needed
  ;;

  type _ Effect.t += AwaitUpdate : 'a 'scope. ('a, 'scope) var -> bool Effect.t

  let fork (f : unit -> unit) : unit =
    try f () with
    | effect AwaitUpdate var, k ->
      let k = dont_leak_please k in
      let f = fun _ -> k.continue true in
      once_inferred f var
  ;;

  let rec await_inferred : 'a 'scope. error_shape:'a -> ('a, 'scope) var -> 'a =
    fun ~error_shape var ->
    let root_data = find_root var in
    match root_data.inferred with
    | Some inferred -> inferred
    | None ->
      if Effect.perform (AwaitUpdate var)
      then await_inferred ~error_shape var
      else error_shape
  ;;

  let compare
    :  'a 'scope.
       ('a -> 'a -> int)
    -> ('scope -> 'scope -> int)
    -> ('a, 'scope) var
    -> ('a, 'scope) var
    -> int
    =
    fun compare_inferred _compare_scope a b ->
    let a = find_root a in
    let b = find_root b in
    if a.recurse_id = b.recurse_id
    then 0
    else (
      match a.inferred, b.inferred with
      | Some a, Some b -> compare_inferred a b
      | Some _, None | None, Some _ | None, None -> Id.compare a.recurse_id b.recurse_id)
  ;;

  let equal
    :  'a 'scope.
       ('a -> 'a -> bool)
    -> ('scope -> 'scope -> bool)
    -> ('a, 'scope) var
    -> ('a, 'scope) var
    -> bool
    =
    fun equal_inferred _equal_scope a b ->
    let a = find_root a in
    let b = find_root b in
    if a.recurse_id = b.recurse_id
    then true
    else (
      let cache = CompareRecurseCache.get () in
      let ids = a.recurse_id, b.recurse_id in
      if cache |> CompareRecurseCache.depth ids > 0
      then true
      else (
        cache |> CompareRecurseCache.enter ids;
        let result =
          match a.inferred, b.inferred with
          | Some a, Some b -> equal_inferred a b
          | Some _, None | None, Some _ | None, None -> false
        in
        cache |> CompareRecurseCache.exit ids;
        result))
  ;;

  let recurse_id var =
    let data = find_root var in
    data.recurse_id
  ;;

  let spans var =
    let data = find_root var in
    data.spans
  ;;

  module Map = struct
    type 'a t =
      { id : Id.t
      ; mutable by_recurse_id : (Id.t, 'a) Hashtbl.t
      }

    let create () = { id = Id.gen (); by_recurse_id = Hashtbl.create 0 }

    let add var value map =
      let rec check_unite current_id data =
        let new_id = data.recurse_id in
        Hashtbl.remove map.by_recurse_id current_id;
        Log.trace (fun log ->
          log
            "Adding id %a, map size = %a"
            Id.print
            new_id
            Int.print
            (Hashtbl.length map.by_recurse_id));
        Hashtbl.add map.by_recurse_id new_id value;
        Log.trace (fun log ->
          log "varmap.add %a to map %a" Id.print new_id Id.print map.id);
        data.on_unite <- check_unite new_id :: data.on_unite
      in
      check_unite (recurse_id var) (find_root var)
    ;;

    let find_opt var map =
      Log.trace (fun log ->
        log "varmap.find_opt %a in map %a" Id.print (recurse_id var) Id.print map.id);
      Hashtbl.find_opt map.by_recurse_id (recurse_id var)
    ;;
  end
end

let fork = Var.fork

type ('a, 'scope) var = ('a, 'scope) Var.t

let equal_var = Var.equal
let compare_var = Var.compare
