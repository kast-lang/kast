open Std
open Kast_util
open Kast_types
module Natives = Natives
module Scope = Scope
include Common

let init : Scope.locals -> state =
 fun values ->
  {
    scope = Scope.with_values ~recursive:false ~parent:None values;
    natives = Natives.init_natives ();
    contexts = Id.Map.empty;
    instantiated_generics = { map = Id.Map.empty };
    cast_impls = { map = Types.ValueMap.empty };
  }

let default () = init Scope.Locals.empty
