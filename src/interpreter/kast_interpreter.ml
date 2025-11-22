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
    natives = Natives.natives;
    contexts = Id.Map.empty;
  }

let default () = init Scope.Locals.empty
