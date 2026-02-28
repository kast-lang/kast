type _ Effect.t += Test : unit Effect.t

let add a b = a + b

let f () =
  let x = add 2 2 in
  Effect.perform Test
;;

let () =
  try f () with
  | effect Test, k -> Effect.Deep.continue k ()
;;
