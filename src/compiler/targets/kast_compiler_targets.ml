open Std
open Kast_util

module Target = struct
  type t =
    | Ir
    | JavaScript
    | C

  let parse target =
    match target with
    | "ir" -> Ir
    | "js" | "javascript" -> JavaScript
    | "c" -> C
    | _ -> fail "Unknown output type %a" String.print_debug target
  ;;
end
