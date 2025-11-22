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
    once_inferred : ('a -> unit) list;
  }

  let new_not_inferred : 'a. unit -> 'a var =
   fun () ->
    {
      state =
        Root
          {
            data =
              { recurse_id = Id.gen (); inferred = None; once_inferred = [] };
          };
    }

  let new_inferred : 'a. 'a -> 'a var =
   fun inferred ->
    {
      state =
        Root
          {
            data =
              {
                recurse_id = Id.gen ();
                inferred = Some inferred;
                once_inferred = [];
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
   fun { recurse_id; inferred; once_inferred } ->
    match inferred with
    | None -> { recurse_id; inferred; once_inferred }
    | Some value ->
        once_inferred |> List.iter (fun f -> f value);
        { recurse_id; inferred; once_inferred = [] }

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit =
   fun print_inferred fmt var ->
    let { recurse_id; inferred; once_inferred = _ } = find_root var in
    match inferred with
    | None -> fprintf fmt "_"
    | Some inferred -> print_inferred fmt inferred

  let unite_data =
   fun ~span unite_inferred
       { recurse_id; inferred = inferred_a; once_inferred = once_inferred_a }
       {
         recurse_id = _;
         inferred = inferred_b;
         once_inferred = once_inferred_b;
       } ->
    let inferred =
      match (inferred_a, inferred_b) with
      | None, None -> None
      | Some inferred, None | None, Some inferred -> Some inferred
      | Some a, Some b -> Some (unite_inferred ~span a b)
    in
    { recurse_id; inferred; once_inferred = once_inferred_a @ once_inferred_b }
    |> check

  let unite : 'a. 'a unite -> 'a var unite =
   fun unite_inferred ~span a b ->
    try
      let root_a = find_root_var a in
      let root_b = find_root_var b in
      if root_a == root_b then a
      else
        let data =
          unite_data ~span unite_inferred (find_root a) (find_root b)
        in
        let root_a, root_b =
          if Random.bool () then (root_a, root_b) else (root_b, root_a)
        in
        root_a.state <- Root { data };
        root_b.state <- NotRoot { closer_to_root = root_a };
        root_a
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
                };
          }
    with effect (Error.Error _ as eff), _k -> Effect.perform eff

  let once_inferred : 'a. ('a -> unit) -> 'a var -> unit =
   fun f var ->
    let root_data = find_root var in
    let root = find_root_var var in
    root.state <-
      Root
        {
          data =
            { root_data with once_inferred = f :: root_data.once_inferred }
            |> check;
        }

  type _ Effect.t += AwaitUpdate : 'a. 'a var -> unit Effect.t

  let fork (type a) (f : unit -> unit) : unit =
    try f ()
    with effect AwaitUpdate var, k ->
      once_inferred (fun _ -> Effect.continue k ()) var

  let rec await_inferred : 'a. 'a var -> 'a =
   fun var ->
    let root_data = find_root var in
    match root_data.inferred with
    | Some inferred -> inferred
    | None ->
        Effect.perform (AwaitUpdate var);
        await_inferred var

  let same a b =
    let a = find_root_var a in
    let b = find_root_var b in
    Repr.phys_equal a b

  let id var =
    let data = find_root var in
    data.recurse_id
end

let fork = Var.fork

type 'a var = 'a Var.t
