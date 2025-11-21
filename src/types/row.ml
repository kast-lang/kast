open Std
open Kast_util
module Inference = Kast_inference_base

type 'a shape =
  | R_Empty
  | R_Cons of {
      label : Label.t;
      value : 'a;
      rest : 'a row;
    }

and 'a t = { var : 'a shape Inference.var }
and 'a row = 'a t

let new_not_inferred () = { var = Inference.Var.new_not_inferred () }
let inferred shape = { var = Inference.Var.new_inferred shape }

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

let print :
    'a.
    options:print_options ->
    (formatter -> 'a -> unit) ->
    formatter ->
    'a row ->
    unit =
 fun ~options print_value fmt row ->
  fprintf fmt "%s" options.open_;
  let first = ref true in
  let rec print_rest ({ var } : 'a row) : unit =
    if not !first then fprintf fmt "%s" options.sep;
    first := false;
    match var |> Inference.Var.inferred_opt with
    | None -> fprintf fmt "%s" options.rest
    | Some R_Empty -> ()
    | Some (R_Cons { label; value; rest }) ->
        fprintf fmt "%s%a%s%a" options.before_label Label.print label
          options.between print_value value;
        print_rest rest
  in
  print_rest row;
  fprintf fmt "%s" options.close

let rec unite_shape : 'a. 'a Inference.unite -> 'a shape Inference.unite =
 fun unite_value ~span a b ->
  let aa = a in
  match (a, b) with
  | R_Empty, R_Empty -> R_Empty
  | R_Empty, R_Cons _ | R_Cons _, R_Empty -> fail "row inference failure"
  | R_Cons a, R_Cons b ->
      if Label.get_name a.label = Label.get_name b.label then
        R_Cons
          {
            label = Label.unite a.label b.label;
            value = unite_value ~span a.value b.value;
            rest = unite unite_value ~span a.rest b.rest;
          }
      else
        let rest = new_not_inferred () in
        a.rest.var
        |> Inference.Var.infer_as ~span (unite_shape unite_value)
             (R_Cons { label = b.label; value = b.value; rest });
        b.rest.var
        |> Inference.Var.infer_as ~span (unite_shape unite_value)
             (R_Cons { label = a.label; value = a.value; rest });
        aa

and unite : 'a. 'a Inference.unite -> 'a row Inference.unite =
 fun unite_value ~span a b ->
  { var = Inference.Var.unite (unite_shape unite_value) ~span a.var b.var }
