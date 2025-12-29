open Std
open Kast_util
module Inference = Kast_inference_base

type ('a, 'scope) shape =
  | R_Empty
  | R_Cons of {
      label : Label.t;
      value : 'a;
      rest : ('a, 'scope) row;
    }
  | R_Error

and ('a, 'scope) t = { var : (('a, 'scope) shape, 'scope) Inference.var }
and ('a, 'scope) row = ('a, 'scope) t [@@deriving eq, ord]

let rec shape_scope :
    'a 'scope.
    (module Inference.Scope with type t = 'scope) ->
    ('a -> 'scope) ->
    ('a, 'scope) shape ->
    'scope =
 fun (type a) (type scope) (module Scope : Inference.Scope with type t = scope)
     (value_scope : a -> scope) (shape : (a, scope) shape) : scope ->
  match shape with
  | R_Empty -> Scope.root ()
  | R_Cons { label = _; value; rest } ->
      let value_scope : scope = value_scope value in
      let rest_scope : scope = scope rest in
      Scope.deepest value_scope rest_scope
  | R_Error -> Scope.root ()

and scope : 'a 'scope. ('a, 'scope) row -> 'scope =
 fun (type a) (type scope) ({ var } : (a, scope) row) -> Inference.Var.scope var

let new_not_inferred ~scope ~span =
  { var = Inference.Var.new_not_inferred ~scope ~span }

let inferred (type scope) (module Scope : Inference.Scope with type t = scope)
    value_scope ~span shape =
  {
    var =
      Inference.Var.new_inferred
        (shape_scope (module Scope) value_scope)
        ~span shape;
  }

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
    'a 'scope.
    options:print_options ->
    (formatter -> 'a -> unit) ->
    formatter ->
    ('a, 'scope) row ->
    unit =
 fun ~options print_value fmt row ->
  fprintf fmt "%s" options.open_;
  let first = ref true in
  let rec print_rest ({ var } : ('a, 'scope) row) : unit =
    let maybe_sep () =
      if not !first then fprintf fmt "%s" options.sep;
      first := false
    in
    match var |> Inference.Var.inferred_opt with
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

let rec unite_shape :
    'a 'scope.
    (module Inference.Scope with type t = 'scope) ->
    ('a -> 'scope) ->
    'a Inference.unite ->
    ('a, 'scope) shape Inference.unite =
 fun (type a) (type scope) (module Scope : Inference.Scope with type t = scope)
     value_scope unite_value ~span a b ->
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
            value = unite_value ~span a.value b.value;
            rest =
              unite (module Scope) value_scope unite_value ~span a.rest b.rest;
          }
      else
        let rest =
          new_not_inferred ~span
            ~scope:(Scope.deepest (scope a.rest) (scope b.rest))
        in
        a.rest.var
        |> Inference.Var.infer_as
             (shape_scope (module Scope) value_scope)
             ~span
             (unite_shape (module Scope) value_scope unite_value)
             Scope.unite
             (R_Cons { label = b.label; value = b.value; rest });
        b.rest.var
        |> Inference.Var.infer_as
             (shape_scope (module Scope) value_scope)
             ~span
             (unite_shape (module Scope) value_scope unite_value)
             Scope.unite
             (R_Cons { label = a.label; value = a.value; rest });
        aa

and unite :
    'a 'scope.
    (module Inference.Scope with type t = 'scope) ->
    ('a -> 'scope) ->
    'a Inference.unite ->
    ('a, 'scope) row Inference.unite =
 fun (type scope) (module Scope : Inference.Scope with type t = scope)
     value_scope unite_value ~span a b ->
  {
    var =
      Inference.Var.unite
        (unite_shape (module Scope) value_scope unite_value)
        Scope.unite ~span a.var b.var;
  }

let rec await_inferred_to_list { var } =
  match var |> Inference.Var.await_inferred ~error_shape:R_Error with
  | R_Error -> []
  | R_Empty -> []
  | R_Cons { label; value; rest } ->
      (label, value) :: await_inferred_to_list rest

let rec of_list :
    'a 'scope.
    (module Inference.Scope with type t = 'scope) ->
    ('a -> 'scope) ->
    span:span ->
    (Label.t * 'a) list ->
    ('a, 'scope) row =
 fun (type a) (type scope) (module Scope : Inference.Scope with type t = scope)
     (value_scope : a -> scope) ~span (list : (Label.t * a) list) ->
  match list with
  | [] -> inferred (module Scope) value_scope ~span R_Empty
  | (label, value) :: rest ->
      inferred (module Scope) value_scope ~span
      <| R_Cons
           {
             label;
             value;
             rest = of_list (module Scope) value_scope ~span rest;
           }
