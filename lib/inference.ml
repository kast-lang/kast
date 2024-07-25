exception FailedUnite of string

module type Checker = sig
  type t

  val unite : t -> t -> t
  val show : t -> string
end

module type T = sig
  type var
  type inferred

  val get_inferred : var -> inferred option
  val get_type : var -> var
  val new_var : unit -> var
  val new_set_var : inferred -> var
  val make_same : var -> var -> unit
  val set : var -> inferred -> unit
  val add_check : var -> (inferred -> bool) -> unit
  val show_id : var -> string
  val get_id : var -> Id.t
end

module Make (Checker : Checker) : T with type inferred := Checker.t = struct
  type inferred = Checker.t

  type inference_data = {
    mutable inferred : Checker.t option;
    mutable checks : (inferred -> bool) list;
    mutable type_var : var option;
  }

  and var_data = Root of inference_data | SameAs of var
  and var = { mutable data : var_data; id : Id.t }

  let new_var () =
    {
      data = Root { inferred = None; type_var = None; checks = [] };
      id = Id.gen ();
    }

  let rec get_root_var : var -> var =
   fun var ->
    match var.data with
    | Root _ -> var
    | SameAs closer_to_root ->
        let root = get_root_var closer_to_root in
        var.data <- SameAs root;
        root

  let show_id var = Id.show (get_root_var var).id
  let get_id var = (get_root_var var).id

  let get_root_data var =
    match var.data with
    | SameAs _ -> failwith "expected a root"
    | Root data -> data

  let get_inferred var =
    let root = get_root_var var in
    let data = get_root_data root in
    data.inferred

  let get_type var =
    let root = get_root_var var in
    let data = get_root_data root in
    match data.type_var with
    | Some t -> t
    | None ->
        let t = new_var () in
        data.type_var <- Some t;
        t

  let unite a b =
    try Checker.unite a b
    with FailedUnite s ->
      failwith @@ "inference unite failure: " ^ s ^ "\na = " ^ Checker.show a
      ^ "\nb = " ^ Checker.show b

  let check_again (data : inference_data) : unit =
    match data.inferred with
    | Some value ->
        data.checks
        |> List.iter (fun f -> if not (f value) then failwith "check failed")
    | None -> ()

  let make_same a b =
    Log.info "make_same 1";
    let a = get_root_var a in
    let b = get_root_var b in
    Log.info "make_same 2";
    if a != b then (
      let a_data = get_root_data a in
      let b_data = get_root_data b in
      let inferred_value =
        match (a_data.inferred, b_data.inferred) with
        | Some inferred, None | None, Some inferred -> Some inferred
        | None, None -> None
        | Some inferred_a, Some inferred_b -> Some (unite inferred_a inferred_b)
      in
      if Random.bool () then (
        a_data.inferred <- inferred_value;
        b.data <- SameAs a)
      else (
        b_data.inferred <- inferred_value;
        a.data <- SameAs b);
      Log.info "make_same 3")

  let set var value =
    let root = get_root_var var in
    let data = get_root_data root in
    match data.inferred with
    | None -> data.inferred <- Some value
    | Some current_value -> data.inferred <- Some (unite current_value value)

  let add_check var f =
    let root = get_root_var var in
    let data = get_root_data root in
    data.checks <- f :: data.checks;
    check_again data

  let new_set_var value =
    let var = new_var () in
    set var value;
    var
end
