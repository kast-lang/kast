open Std

module type S = sig
  type t

  module Shape : sig
    type t = ..

    module Error : sig
      type t += T : t
    end
  end

  val error : unit -> t
  val print : formatter -> t -> unit
end
