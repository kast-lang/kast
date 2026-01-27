open Std
open Kast_util

module Target = struct
  type t =
    | Ir
    | JavaScript

  let parse target =
    match target with
    | "ir" -> Ir
    | "js" | "javascript" -> JavaScript
    | _ -> fail "Unknown output type %a" String.print_debug target
  ;;
end
