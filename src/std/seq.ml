module Seq = struct
  include Stdlib.Seq

  let last : 'a. 'a t -> 'a option =
   fun seq ->
    let result = ref None in
    seq |> iter (fun item -> result := Some item);
    !result
end
