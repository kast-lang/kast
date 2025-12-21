open Kast_types

type evaled_place_expr =
  | Place of (mut:bool * place)
  | RefBlocked of Types.blocked_value
