open Prelude
open Types

module Make (Inference : Modules.Inference) (Show : Modules.Show) = struct
  let empty_type_var_map () : type_var_map ref = ref Id.Map.empty

  let rec update_locals (prev : local StringMap.t)
      (new_locals : local StringMap.t) : local StringMap.t =
    StringMap.union
      (fun _name _prev new_value -> Some new_value)
      prev new_locals

  and pattern_to_value_with (f : binding -> value) (p : pattern) : value =
    match p with
    | Binding { data = _; binding } -> f binding
    | Void { data = _ } -> Void
    | Placeholder { data } ->
        InferVar
          (let var = Inference.new_var () in
           Inference.set data.type_var (Type (Inference.get_type var));
           var)
    | Dict { fields; data = _ } ->
        Dict { fields = fields |> StringMap.map (pattern_to_value_with f) }
    | Variant { data; name; value } ->
        Variant
          {
            typ = InferVar data.type_var;
            name;
            value = Option.map (pattern_to_value_with f) value;
          }
    | Union { data = _; a; b = _ } -> pattern_to_value_with f a

  and value_to_type : value -> value_type = function
    | InferVar var -> (
        match Inference.get_inferred var with
        | Some value -> value_to_type value
        | None ->
            let t : value_type = InferVar (Inference.new_var ()) in
            Inference.set var (Type t : value);
            t)
    | Void -> Void
    | Type t -> t
    | Dict { fields } -> Dict { fields = StringMap.map value_to_type fields }
    | Struct { data; _ } ->
        Dict
          {
            fields =
              data.locals
              |> StringMap.map (fun { value; _ } -> value_to_type value);
          }
    | Template f -> Template f
    | Var { id; typ } ->
        ignore @@ Inference.unite_types typ Type;
        Var { id }
    | Binding binding -> Binding binding
    | other -> failwith (Show.show other ^ " is not a type")
end
