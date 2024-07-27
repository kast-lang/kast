open Prelude
open Types

module Make
    (TypeId : Modules.TypeId)
    (Show : Modules.Show)
    (Interpreter : Modules.Interpreter)
    (Inference : Modules.Inference)
    (Utils : Modules.Utils) : Modules.Cast = struct
  open Show
  open Interpreter
  open Utils

  let trait_impls : (id, (id, value) Hashtbl.t) Hashtbl.t = Hashtbl.create 0

  let rec find_trait_id (trait : value) : id =
    match trait with
    | Template trait -> trait.id
    | Type trait -> TypeId.get trait
    | _ -> failwith @@ "this value can not be a trait: " ^ show trait

  and add_rule (ty : value) ~(trait : value) ~(impl : value) : unit =
    let ty = value_to_type ty in
    let trait_id : Id.t = find_trait_id trait in
    let type_id = TypeId.get ty in
    if Hashtbl.find_opt trait_impls type_id |> Option.is_none then
      Hashtbl.add trait_impls type_id (Hashtbl.create 0);
    let type_impls = Hashtbl.find trait_impls type_id in
    Hashtbl.add type_impls trait_id impl;
    Log.trace @@ "added impl " ^ show trait ^ " for " ^ show_type ty ^ " as "
    ^ show impl

  and check (value : value) ~trait : bool =
    let trait_id = find_trait_id trait in
    let check (value : value) =
      match
        Hashtbl.find_opt trait_impls (TypeId.get @@ value_to_type value)
      with
      | Some impls -> (
          match Hashtbl.find_opt impls trait_id with
          | Some _ -> true
          | None -> false)
      | None -> false
    in
    match value with
    | Type (InferVar var) ->
        Inference.add_check var check;
        true
    | _ -> check value

  and perform (value : value) ~(trait : value) : value =
    match trait with
    | Type Type -> Type (value_to_type value)
    | _ -> (
        let ty = value_to_type value in
        let show_impls (ty : value_type) (tid : id)
            (impls : (id, value) Hashtbl.t) =
          Hashtbl.iter
            (fun trait impl ->
              Log.trace @@ "impl " ^ "trait (id=" ^ Id.show trait ^ ") for "
              ^ show_type ty ^ " as " ^ show impl)
            impls
        in
        let fail s =
          (* show_all_impls (); *)
          (* Log.trace s; *)
          failwith s
        in
        match Hashtbl.find_opt trait_impls (TypeId.get ty) with
        | Some impls -> (
            match Hashtbl.find_opt impls (find_trait_id trait) with
            | Some impl -> impl
            | None ->
                Log.error @@ "get_impl failed: " ^ show trait
                ^ " is not implemented for " ^ show_type ty
                ^ " (see existing impls above)";
                failwith "get_impl")
        | None ->
            Log.error @@ "get_impl failed: " ^ show trait
            ^ " is not implemented for " ^ show_type ty ^ " (no impls found)";
            failwith "get_impl")
end
