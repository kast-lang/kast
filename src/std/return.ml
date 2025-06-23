module Return = struct
  type 'a return_handle = { return : 'never. 'a -> 'never }

  let return_to : 'r 'never. 'r return_handle -> 'r -> 'never =
   fun handle value -> handle.return value

  let with_return (type r) (f : r return_handle -> r) =
    let exception Return of r in
    let handle = { return = (fun value -> raise @@ Return value) } in
    try f handle with Return x -> x
end

include Return
