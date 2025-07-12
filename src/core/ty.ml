open Std

module T = struct
  type t = ..
end

include T
include Print.Make (T)
