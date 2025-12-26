type a =
  | A_Empty
  | A_B of b

and b =
  | B_Empty
  | B_A of a

module Print = struct
  let rec print_a : a -> unit = function
    | A_Empty -> ()
    | A_B b -> print_b b

  and print_b : b -> unit = function
    | B_Empty -> ()
    | B_A a -> print_a a
end

module Inference = struct
  let rec unite_a : a -> a -> a =
   fun a b ->
    match (a, b) with
    | A_Empty, A_Empty -> A_Empty
    | A_Empty, _ -> failwith "can't unite"
    | A_B a, A_B b -> A_B (unite_b a b)
    | A_B _, _ -> failwith "can't unite"

  and unite_b : b -> b -> b =
   fun a b ->
    match (a, b) with
    | B_Empty, B_Empty -> B_Empty
    | B_Empty, _ -> failwith "can't unite"
    | B_A a, B_A b -> B_A (unite_a a b)
    | B_A _, _ -> failwith "can't unite"
end
