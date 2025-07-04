module Effect = struct
  include Stdlib.Effect

  let continue = Deep.continue
  let discontinue = Deep.discontinue

  let continue_with (type a) (type b) (k : (a, b) continuation) (f : unit -> a)
      : b =
    let result = try Either.Right (f ()) with exc -> Either.Left exc in
    match result with
    | Right value -> Deep.continue k value
    | Left exc -> Deep.discontinue k exc
end
