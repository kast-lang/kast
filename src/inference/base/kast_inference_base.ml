open Std
open Kast_util
module Error = Error

type 'a unite = span:span -> 'a -> 'a -> 'a

module Var = struct
  type 'a var = { mutable state : 'a var_state }

  and 'a var_state =
    | Root of { data : 'a var_data }
    | NotRoot of { closer_to_root : 'a var }

  and 'a var_data = {
    recurse_id : id;
    inferred : 'a option;
    spans : SpanSet.t;
    mutable once_inferred : ('a -> unit) list;
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
                once_inferred = [];
                spans = SpanSet.singleton span;
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
                inferred = Some inferred;
                once_inferred = [];
                spans = SpanSet.singleton span;
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

  type 'a t = 'a var

  let check : 'a. 'a var_data -> 'a var_data =
   fun data ->
    match data.inferred with
    | None -> data
    | Some value ->
        let fs = data.once_inferred in
        data.once_inferred <- [];
        fs |> List.iter (fun f -> f value);
        data

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit =
   fun print_inferred fmt var ->
    let { recurse_id = _; inferred; once_inferred = _; spans = _ } =
      find_root var
    in
    match inferred with
    | None -> fprintf fmt "_"
    | Some inferred -> print_inferred fmt inferred

  let unite_data =
   fun ~span unite_inferred
       ({
          recurse_id;
          inferred = inferred_a;
          once_inferred = once_inferred_a;
          spans = spans_a;
        } as data_a)
       ({
          recurse_id = _;
          inferred = inferred_b;
          once_inferred = once_inferred_b;
          spans = spans_b;
        } as data_b) ->
    let inferred =
      match (inferred_a, inferred_b) with
      | None, None -> None
      | Some inferred, None | None, Some inferred -> Some inferred
      | Some a, Some b -> Some (unite_inferred ~span a b)
    in
    data_a.once_inferred <- [];
    data_b.once_inferred <- [];
    {
      recurse_id;
      inferred;
      once_inferred = once_inferred_a @ once_inferred_b;
      spans = SpanSet.union spans_a spans_b;
    }
    |> check

  let rec unite : 'a. 'a unite -> 'a var unite =
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
        root
    with effect (Error.Error _ as eff), _k ->
      Effect.perform eff;
      a

  let infer_as : 'a. 'a unite -> span:span -> 'a -> 'a var -> unit =
   fun unite_inferred ~span infer_as var ->
    try
      let root_data = find_root var in
      let root = find_root_var var in
      root.state <-
        Root
          {
            data =
              unite_data ~span unite_inferred root_data
                {
                  recurse_id = Id.gen ();
                  inferred = Some infer_as;
                  once_inferred = [];
                  spans = SpanSet.singleton span;
                };
          }
    with effect (Error.Error _ as eff), _k -> Effect.perform eff

  let once_inferred : 'a. ('a -> unit) -> 'a var -> unit =
   fun f var ->
    let root_data = find_root var in
    root_data.once_inferred <- f :: root_data.once_inferred

  type _ Effect.t += AwaitUpdate : 'a. 'a var -> bool Effect.t

  let fork (type a) (f : unit -> unit) : unit =
    try f ()
    with effect AwaitUpdate var, k ->
      once_inferred (fun _ -> Effect.continue k true) var

  let rec await_inferred : 'a. error_shape:'a -> 'a var -> 'a =
   fun ~error_shape var ->
    let root_data = find_root var in
    match root_data.inferred with
    | Some inferred -> inferred
    | None ->
        if Effect.perform (AwaitUpdate var) then await_inferred ~error_shape var
        else error_shape

  let equal a b =
    let a = find_root_var a in
    let b = find_root_var b in
    Repr.phys_equal a b

  let compare a b =
    let a = find_root a in
    let b = find_root b in
    Id.compare a.recurse_id b.recurse_id

  let recurse_id var =
    let data = find_root var in
    data.recurse_id

  let spans var =
    let data = find_root var in
    data.spans
end

let fork = Var.fork

type 'a var = 'a Var.t

let equal_var _ a b = Var.equal a b
let compare_var _ a b = Var.compare a b
