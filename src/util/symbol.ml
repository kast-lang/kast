open Std
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
  let equal a b = Id.equal a.id b.id
  let hash symbol = Id.hash symbol.id

  let print : formatter -> symbol -> unit =
   fun fmt symbol -> fprintf fmt "%a" String.print_maybe_escaped symbol.name
end

module SymbolMap = Map.Make (Symbol)

type symbol = Symbol.t
