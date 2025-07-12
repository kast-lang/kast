module T = struct
  type _ t = ..
end

include T
include Print.MakeGadt (T)
