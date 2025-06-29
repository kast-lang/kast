open Std

module Var = struct
  type 'a var = { mutable state : 'a var_state }

  and 'a var_state =
    | Root of { data : 'a var_data }
    | NotRoot of { closer_to_root : 'a var }

  and 'a var_data = { mutable inferred : 'a option }

  let new_not_inferred : 'a. unit -> 'a var =
   fun () -> { state = Root { data = { inferred = None } } }

  let new_inferred : 'a. 'a -> 'a var =
   fun inferred -> { state = Root { data = { inferred = Some inferred } } }

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

  type 'a t = 'a var

  let print : 'a. (formatter -> 'a -> unit) -> formatter -> 'a var -> unit =
   fun print_inferred fmt var ->
    let { inferred } = find_root var in
    match inferred with
    | None -> fprintf fmt "@{<italic><not inferred>@}"
    | Some inferred -> print_inferred fmt inferred

  and unite : 'a. ('a -> 'a -> 'a) -> 'a var -> 'a var -> 'a var =
   fun unite_inferred a b -> failwith __LOC__
end

type 'a var = 'a Var.t
