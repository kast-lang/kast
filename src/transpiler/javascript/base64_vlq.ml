open Std

let base64_digit : int -> char =
 fun x ->
  if x < 26 then Char.chr (Char.code 'A' + x)
  else if x < 52 then Char.chr (Char.code 'a' + x - 26)
  else if x < 62 then Char.chr (Char.code '0' + x - 52)
  else if x = 62 then '+'
  else if x = 63 then '/'
  else fail "Tried to use >=64 for base64"

let continuation_bit = Int32.shift_left Int32.one 5

let print_base64_vlq : formatter -> int -> unit =
 fun fmt x ->
  let x = Int32.of_int x in
  let write_digit digit =
    fprintf fmt "%c" (base64_digit (Int32.to_int digit))
  in
  let rec go x =
    let data, rest =
      ( Int32.logand x (Int32.sub (Int32.shift_left Int32.one 5) Int32.one),
        Int32.shift_right_logical x 5 )
    in
    if rest <> Int32.zero then (
      write_digit (Int32.logor continuation_bit data);
      go rest)
    else write_digit data
  in
  let x =
    Int32.logor
      (Int32.shift_left (Int32.abs x) 1)
      (Int32.shift_right_logical x 31)
  in
  go x
