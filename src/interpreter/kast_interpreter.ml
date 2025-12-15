open Std
open Kast_util
open Kast_types
module Natives = Natives
module Scope = Scope
include Common

let init : Types.name_part -> Scope.locals -> state =
 fun name_part values ->
  {
    scope = Scope.with_values ~recursive:false ~parent:None values;
    current_fn_natives = Hashtbl.create 0;
    natives = Natives.init_natives ();
    contexts = Id.Map.empty;
    instantiated_generics = { map = Id.Map.empty };
    cast_impls =
      { map = Types.ValueMap.empty; as_module = Types.ValueMap.empty };
    current_name_parts_rev = [ name_part ];
  }

let default name_part = init name_part Scope.Locals.empty
