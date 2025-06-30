open Source
open Id

module Symbol = struct
  type t = {
    id : id;
    name : string;
  }

  type symbol = t

  let create name = { id = Id.gen (); name }
  let compare a b = Id.compare a.id b.id
end

module SymbolMap = Map.Make (Symbol)

type symbol = Symbol.t
