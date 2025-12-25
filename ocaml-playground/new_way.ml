module ModuleTypes = struct
  module A = struct
    module type Deps = sig
      module B : sig
        type t
      end
    end

    module type S = sig
      module Deps : Deps

      type t =
        | A_Empty
        | A_B of Deps.B.t

      val print : t -> unit
      val unite : t -> t -> t
    end
  end

  module B = struct
    module type Deps = sig
      module A : sig
        type t
      end
    end

    module type S = sig
      module Deps : Deps

      type t =
        | B_Empty
        | B_A of Deps.A.t

      val print : t -> unit
      val unite : t -> t -> t
    end
  end
end

module NonRec = struct
  module A = struct
    module type S = ModuleTypes.A.S

    module Make (B : ModuleTypes.B.S) : S = struct
      module Deps = struct
        module B = B
      end

      type t =
        | A_Empty
        | A_B of B.t

      let print = function
        | A_Empty -> ()
        | A_B b -> B.print b

      let unite a b =
        match (a, b) with
        | A_Empty, A_Empty -> A_Empty
        | A_Empty, _ -> failwith "can't unite"
        | A_B a, A_B b -> A_B (B.unite a b)
        | A_B _, _ -> failwith "can't unite"
    end
  end

  module B = struct
    module type S = ModuleTypes.B.S

    module Make (A : ModuleTypes.A.S) : S = struct
      module Deps = struct
        module A = A
      end

      type t =
        | B_Empty
        | B_A of Deps.A.t

      let print = function
        | B_Empty -> ()
        | B_A a -> Deps.A.print a

      let unite a b =
        match (a, b) with
        | B_Empty, B_Empty -> B_Empty
        | B_Empty, _ -> failwith "can't unite"
        | B_A a, B_A b -> B_A (Deps.A.unite a b)
        | B_A _, _ -> failwith "can't unite"
    end
  end
end

module rec A : NonRec.A.S = NonRec.A.Make (B)
and B : NonRec.B.S = NonRec.B.Make (A)
