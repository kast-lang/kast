open Std
open Kast_util
module Inference = Kast_inference_base

type label_data = {
  id : Id.t;
  name : string;
  definition : span option;
  mutable references : span list;
}

let equal_label_data a b = Id.equal a.id b.id
let compare_label_data a b = Id.compare a.id b.id

type label = { var : label_data Inference.var }
type t = label

let equal a b = Inference.equal_var equal_label_data a.var b.var
let compare a b = Inference.compare_var compare_label_data a.var b.var

let create_definition span name =
  {
    var =
      Inference.Var.new_inferred span
        { id = Id.gen (); name; definition = Some span; references = [] };
  }

let create_reference span name =
  {
    var =
      Inference.Var.new_inferred span
        { id = Id.gen (); name; definition = None; references = [ span ] };
  }

let get_name { var } =
  let data = var |> Inference.Var.inferred_opt |> Option.get in
  data.name

let get_data_span data =
  data.definition
  |> Option.unwrap_or_else (fun () -> data.references |> List.head)

let get_span { var } =
  var |> Inference.Var.inferred_opt |> Option.get |> get_data_span

let get_data { var } = var |> Inference.Var.inferred_opt |> Option.get

let unite_data ~span:_ a b =
  let name =
    match (a.name, b.name) with
    (* TODO *)
    | "<TODO>", name | name, "<TODO>" -> name
    | _ ->
        if a.name <> b.name then
          fail "tried uniting different labels %S (%a) and %S (%a)" a.name
            Span.print (get_data_span a) b.name Span.print (get_data_span b)
        else a.name
  in
  let result =
    {
      id = a.id;
      name;
      definition = a.definition |> Option.or_ b.definition;
      references = a.references @ b.references;
    }
  in
  (match (a.definition, b.definition) with
  | Some _a, Some b -> result.references <- b :: result.references
  | _ -> ());
  (* TODO performance *)
  result.references <-
    result.references
    |> List.sort_uniq (fun a b -> if a < b then -1 else if a > b then 1 else 0);
  result

let unite =
  let span = Span.fake "_" in
  fun a b -> { var = Inference.Var.unite unite_data ~span a.var b.var }

let add_reference span label =
  let reference = create_reference span (get_name label) in
  ignore <| unite reference label

let print fmt label =
  match label.var |> Inference.Var.inferred_opt with
  | Some data -> fprintf fmt "%s" data.name
  | None -> fprintf fmt "_"
