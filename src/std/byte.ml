open Char
open Int

module Byte = struct
  type t = char

  let of_int i = Char.chr (((i mod 256) + 256) mod 256)
  let to_int = Char.code
  let bin_op op_i a b = op_i (Char.code a) (Char.code b) |> of_int
  let ( + ) = bin_op ( + )
  let ( - ) = bin_op ( - )
  let ( * ) = bin_op ( * )
  let ( / ) = bin_op ( / )
  let ( mod ) = bin_op ( mod )
  let logand = bin_op Int.logand
  let logor = bin_op Int.logor
  let logxor = bin_op Int.logxor
  let shift_left = bin_op Int.shift_left
  let shift_right = bin_op Int.shift_right
  let lognot b = b |> Char.code |> Int.lognot |> of_int
end
