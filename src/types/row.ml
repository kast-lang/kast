open Std
open Kast_util
module Inference = Kast_inference_base

module type S = sig
  type t
end

module Make (Value : Inference.Inferrable) : S = struct
  module rec Shape : sig
    type t =
      | R_Empty
      | R_Cons of {
          label : Label.t;
          value : Value.t;
          rest : T.t;
        }
      | R_Error

    module Scope : Inference.Scope

    val scope : t -> Scope.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val unite : t Inference.unite
    val error : unit -> t
  end = struct
    type t =
      | R_Empty
      | R_Cons of {
          label : Label.t;
          value : Value.t;
          rest : T.t;
        }
      | R_Error
    [@@deriving eq, ord]

    module Scope = Value.Scope

    let scope = function
      | R_Empty -> Scope.root ()
      | R_Error -> Scope.root ()
      | R_Cons { label = _; value; rest } ->
          Scope.common (Value.scope value) (T.scope rest)

    let error () = R_Error

    let unite ~span a b =
      let scope = Value.Scope.common (scope a) (scope b) in
      let aa = a in
      match (a, b) with
      | R_Error, smth | smth, R_Error -> smth
      | R_Empty, R_Empty -> R_Empty
      | R_Empty, R_Cons _ | R_Cons _, R_Empty ->
          Inference.Error.error span "row inference failure";
          R_Empty
      | R_Cons a, R_Cons b ->
          if Label.get_name a.label = Label.get_name b.label then
            R_Cons
              {
                label = Label.unite a.label b.label;
                value = Value.unite ~span a.value b.value;
                rest = T.unite ~span a.rest b.rest;
              }
          else
            let rest = T.new_not_inferred ~span ~scope in
            a.rest.var
            |> Var.infer_as ~span
                 (R_Cons { label = b.label; value = b.value; rest });
            b.rest.var
            |> Var.infer_as ~span
                 (R_Cons { label = a.label; value = a.value; rest });
            aa
  end

  and T : sig
    type t = { var : Var.t }

    val scope : t -> Value.Scope.t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val unite : t Inference.unite
    val new_inferred : span:span -> Shape.t -> t
    val new_not_inferred : span:span -> scope:Value.Scope.t -> t
  end = struct
    type t = { var : Var.t }

    let scope { var } = Var.scope var
    let equal a b = Var.equal a.var b.var
    let compare a b = Var.compare a.var b.var
    let unite ~span a b = { var = Var.unite ~span a.var b.var }
    let new_inferred ~span shape = { var = Var.new_inferred ~span shape }

    let new_not_inferred ~span ~scope =
      { var = Var.new_not_inferred ~span ~scope }
  end

  and Var : (Inference.Var.S with type Value.t = Shape.t) =
    Inference.Var.Make (Shape)

  include T

  type print_options = {
    open_ : string;
    close : string;
    sep : string;
    before_label : string;
    between : string;
    rest : string;
  }

  let default_print_options : print_options =
    {
      open_ = "(";
      sep = ",";
      before_label = ".";
      between = " = ";
      close = ")";
      rest = "...";
    }

  let print print_value ~options fmt row =
    fprintf fmt "%s" options.open_;
    let first = ref true in
    let rec print_rest row : unit =
      let maybe_sep () =
        if not !first then fprintf fmt "%s" options.sep;
        first := false
      in
      let shape = row.var |> Var.inferred_opt in
      match shape with
      | None ->
          maybe_sep ();
          fprintf fmt "%s" options.rest
      | Some R_Error ->
          maybe_sep ();
          fprintf fmt "<error>"
      | Some R_Empty -> ()
      | Some (R_Cons { label; value; rest }) ->
          maybe_sep ();
          fprintf fmt "%s%a%s%a" options.before_label Label.print label
            options.between print_value value;
          print_rest rest
    in
    print_rest row;
    fprintf fmt "%s" options.close

  let rec await_inferred_to_list row =
    match row.var |> Var.await_inferred with
    | R_Error -> []
    | R_Empty -> []
    | R_Cons { label; value; rest } ->
        (label, value) :: await_inferred_to_list rest

  let rec of_list ~span list =
    match list with
    | [] -> new_inferred ~span R_Empty
    | (label, value) :: rest ->
        new_inferred ~span <| R_Cons { label; value; rest = of_list ~span rest }
end
