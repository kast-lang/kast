open Format

module Int = struct
  include Stdlib.Int

  let print : formatter -> int -> unit = fun fmt x -> fprintf fmt "%d" x
end
