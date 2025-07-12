module Bool = Bool
module Int32 = Int32
module String = String
module Tuple = Tuple
module Fn = Fn
module TypeAscribe = Type_ascribe

let init () =
  Bool.init ();
  Int32.init ();
  String.init ();
  Tuple.init ();
  Fn.init ();
  TypeAscribe.init ()
