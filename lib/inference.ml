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
  val new_var : unit -> var
  val make_same : var -> var -> unit
  val set : var -> inferred -> unit
end

module Make (Checker : Checker) : T with type inferred := Checker.t = struct
  type inferred = Checker.t
  type inference_data = { mutable inferred : Checker.t option }

  type var_data = Root of inference_data | SameAs of var
  and var = { mutable data : var_data; id : Id.t }

  let new_var () = { data = Root { inferred = None }; id = Id.gen () }

  let rec get_root_var : var -> var =
   fun var ->
    match var.data with
    | Root _ -> var
    | SameAs closer_to_root ->
        let root = get_root_var closer_to_root in
        var.data <- SameAs root;
        root

  let get_root_data var =
    match var.data with
    | SameAs _ -> failwith "expected a root"
    | Root data -> data

  let get_inferred var =
    let root = get_root_var var in
    let data = get_root_data root in
    data.inferred

  let unite a b =
    try Checker.unite a b
    with FailedUnite s ->
      failwith @@ "inference unite failure: " ^ s ^ "\na = " ^ Checker.show a
      ^ "\nb = " ^ Checker.show b

  let make_same a b =
    let a = get_root_var a in
    let b = get_root_var b in
    if a != b then
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
        a.data <- SameAs b)

  let set var value =
    let root = get_root_var var in
    let data = get_root_data root in
    match data.inferred with
    | None -> data.inferred <- Some value
    | Some current_value -> data.inferred <- Some (unite current_value value)
end
