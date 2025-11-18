open Std
open Kast_util
module Inference = Kast_inference_base

type label_data = {
  name : string;
  definition : span option;
  mutable references : span list;
}

type label = { var : label_data Inference.var }
type t = label

let create_definition span name =
  {
    var =
      Inference.Var.new_inferred
        { name; definition = Some span; references = [] };
  }

let create_reference span name =
  {
    var =
      Inference.Var.new_inferred
        { name; definition = None; references = [ span ] };
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

let unite a b =
  { var = Inference.Var.unite unite_data ~span:(Span.fake "_") a.var b.var }

let add_reference span label =
  let reference = create_reference span (get_name label) in
  ignore <| unite reference label
