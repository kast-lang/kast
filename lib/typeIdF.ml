open Prelude
open Types

module Make (Inference : Modules.Inference) : Modules.TypeId = struct
  let unwind_token = Id.gen ()
  let delimited_token = Id.gen ()
  let never = Id.gen ()
  let ast = Id.gen ()
  let ir = Id.gen ()
  let void = Id.gen ()
  let bool = Id.gen ()
  let int32 = Id.gen ()
  let int64 = Id.gen ()
  let float32 = Id.gen ()
  let float64 = Id.gen ()
  let string = Id.gen ()
  let ty = Id.gen ()
  let id_to_type : (id, value_type) Hashtbl.t = Hashtbl.create 0

  module type MapKey = sig
    type t
  end

  module MakeMap (Key : MapKey) = struct
    type key = Key.t

    let key_to_id = Hashtbl.create 0

    let get_id (key : key) =
      match Hashtbl.find_opt key_to_id key with
      | Some id -> id
      | None ->
          let id = Id.gen () in
          Hashtbl.add key_to_id key id;
          id
  end

  type dict_key = { fields : id StringMap.t }

  module Dict = MakeMap (struct
    type t = dict_key
  end)

  type fn_key = { args : id; result : id; contexts : id }

  module Fn = MakeMap (struct
    type t = fn_key
  end)

  (* Most of the good programmers do programming not because they expect to get paid or get adulation by the public,
     but because it is fun to program.
     - Linus Torvalds *)

  let rec get : value_type -> id =
   fun t ->
    let id =
      match t with
      | Binding { id; _ } -> id
      | UnwindToken -> unwind_token
      | DelimitedToken -> delimited_token
      | Never -> never
      | Ast -> ast
      | Ir -> ir
      | Void -> void
      | Bool -> bool
      | Int32 -> int32
      | Int64 -> int64
      | Float32 -> float32
      | Float64 -> float64
      | String -> string
      | Fn f ->
          Fn.get_id
            {
              args = get f.arg_type;
              result = get f.result_type;
              contexts = void (* TODO type_id f.contexts *);
            }
      | Macro f -> failwith "todo typeid macro"
      | Template f -> failwith "todo typeid template"
      | BuiltinMacro -> failwith "todo typeid builtin_macro"
      | Dict { fields } -> Dict.get_id { fields = fields |> StringMap.map get }
      | Type -> ty
      | Union _ -> failwith "todo typeid union"
      | OneOf variants -> failwith "todo typeid oneof"
      | NewType inner -> failwith "todo typeid newtype"
      | Var { id } -> id
      | InferVar var -> (
          match (Inference.get_inferred var : value option) with
          | Some (Type inferred) -> get inferred
          | Some _ -> failwith "type was inferred as not a type wtf"
          | None -> failwith "can't get id for not inferred types")
      | MultiSetOldToRemove _ -> failwith "todo typeid multiset"
      | MultiSet _ -> failwith "todo typeid multiset"
    in
    Hashtbl.add id_to_type id t;
    id

  let to_type (id : id) : value_type = Hashtbl.find id_to_type id
end
