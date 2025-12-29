open Std
open Kast_util
open Kast_types
include Kast_interpreter_core
module Natives = Kast_interpreter_natives

let init : Types.name_part -> Scope.locals -> state =
 fun name_part values ->
  {
    scope =
      Scope.with_values ~span:(Span.fake "<root>") ~recursive:false ~parent:None
        values;
    result_scope = None;
    current_fn_natives = Hashtbl.create 0;
    natives = Natives.init_natives ();
    contexts = Id.Map.empty;
    instantiated_generics = { map = Id.Map.empty };
    cast_impls =
      { map = Types.ValueMap.empty; as_module = Types.ValueMap.empty };
    current_name = Simple name_part;
  }

let default name_part = init name_part Scope.Locals.empty
