include Abstract_expr.Make (Value)
module Ty = Abstract_expr.Make (Ty)

let init () =
  init ();
  Ty.init ()
