type value = Int of int

let show = function Int value -> string_of_int value
let add a b = match (a, b) with Int a, Int b -> Int (a + b)

let rec eval : Ast.t -> value = function
  | Ast.Unit s -> Int (int_of_string s)
  | Ast.List list -> (
      match list with
      | [ single ] -> eval single
      | [ Ast.Unit "+"; a; b ] -> add (eval a) (eval b)
      | _ -> raise (Failure "wut"))
