module Bool = struct
  include Stdlib.Bool

  let then_some (type a) (value : a) (x : bool) : a option =
    match x with
    | true -> Some value
    | false -> None
end
