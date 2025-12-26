open Kast_types

type evaled_place_expr =
  | Place of (mut:bool * Place.t)
  | RefBlocked of Types.blocked_value
