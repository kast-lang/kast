module ModuleTypes = struct
  module type A = sig end
  module type B = sig end
end

module NonRec = struct
  module A = struct
    module type S = ModuleTypes.A

    module Make (B : ModuleTypes.B) : S = struct
      type t = unit
    end
  end

  module B = struct
    module type S = sig end

    module Make (A : ModuleTypes.A) : S = struct end
  end
end

module rec A : NonRec.A.S = NonRec.A.Make (B)
and B : NonRec.B.S = NonRec.B.Make (A)
