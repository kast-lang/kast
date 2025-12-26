module New_way = New_way
module Old_way = Old_way

type nat =
  | Zero
  | Succ of nat

let rec x : nat = Succ x

let rec print_nat : nat -> unit = function
  | Zero -> print_endline "ZERO"
  | Succ x ->
      print_endline "Succ";
      print_nat x

let () = print_nat x
