type _ Effect.t += Foo : unit -> unit Effect.t
type _ Effect.t += Foo2 : unit -> unit Effect.t

let fork f =
  try f ()
  with effect Foo (), k ->
    print_endline "handled in fork";
    Effect.Deep.continue k ()

let fork2 f =
  fork (fun () ->
      try f ()
      with effect Foo2 (), k ->
        print_endline "handled in fork2";
        Effect.Deep.continue k ())

let () =
  print_endline "Hello3";
  fork2 (fun () -> Effect.perform (Foo ()))
