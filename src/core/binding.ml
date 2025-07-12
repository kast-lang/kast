open Std
open Kast_util

type t = {
  name : Symbol.t;
  span : span;
  ty : Ty.t;
  mutable references : span list;
}

let print (fmt : formatter) (binding : t) : unit = fprintf fmt "<todo>"
