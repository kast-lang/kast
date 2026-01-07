open Std
open Kast_transpiler_javascript.Base64_vlq

(*
  000001
  -0
*)

let () = println "%a" print_base64_vlq 2147483647
