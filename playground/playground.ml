open Js_of_ocaml
open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int t

let comp1 () = perform (Xchg 0) + perform (Xchg 1)
let f () = try comp1 () with effect Xchg n, k -> continue k (n + 1)

let () =
  let console = Console.console in
  let text = Format.sprintf "Calculated with effects: %d" (f ()) in
  console##log (Js.string text)
