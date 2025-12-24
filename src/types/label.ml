open Std
open Kast_util
module Inference = Kast_inference_base

module LabelData = struct
  type t = {
    id : Id.t;
    name : string;
    definition : span option;
    mutable references : span list;
  }

  module Scope = Inference.NoScope

  let scope (_ : t) = Scope.root ()
  let equal a b = Id.equal a.id b.id
  let compare a b = Id.compare a.id b.id

  let get_span data =
    data.definition
    |> Option.unwrap_or_else (fun () -> data.references |> List.head)

  let unite : t Inference.unite =
   fun ~span:_ a b ->
    let name =
      match (a.name, b.name) with
      (* TODO *)
      | "<TODO>", name | name, "<TODO>" -> name
      | _ ->
          if a.name <> b.name then
            fail "tried uniting different labels %S (%a) and %S (%a)" a.name
              Span.print (get_span a) b.name Span.print (get_span b)
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
      |> List.sort_uniq (fun a b ->
          if a < b then -1 else if a > b then 1 else 0);
    result

  let error () = failwith __LOC__
end

module Var = Inference.Var.Make (LabelData)

type label = { var : Var.t }
type t = label

let equal a b = Var.equal a.var b.var
let compare a b = Var.compare a.var b.var

let create_definition span name =
  {
    var =
      Var.new_inferred ~span
        { id = Id.gen (); name; definition = Some span; references = [] };
  }

let create_reference span name =
  {
    var =
      Var.new_inferred ~span
        { id = Id.gen (); name; definition = None; references = [ span ] };
  }

let get_name { var } =
  let data = var |> Var.inferred_opt |> Option.get in
  data.name

let get_span { var } =
  var |> Var.inferred_opt |> Option.get |> LabelData.get_span

let get_data { var } = var |> Var.inferred_opt |> Option.get

let unite =
  let span = Span.fake "_" in
  fun a b -> { var = Var.unite ~span a.var b.var }

let add_reference span label =
  let reference = create_reference span (get_name label) in
  ignore <| unite reference label

let print fmt label =
  match label.var |> Var.inferred_opt with
  | Some data -> fprintf fmt "%s" data.name
  | None -> fprintf fmt "_"
