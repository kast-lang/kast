type _ Effect.t += Foo : unit Effect.t

exception Cancel

let dont_leak_please k =
  Gc.finalise
    (fun k ->
      print_endline "hmmm";
      Effect.Deep.discontinue k Cancel)
    k

let () =
  while true do
    try
      let s = String.init 1000 (fun i -> if i mod 2 = 0 then 'a' else 'b') in
      Effect.perform Foo;
      print_endline s
    with
    | effect Foo, k ->
        let k = dont_leak_please k in
        ()
    | Cancel -> ()
  done
