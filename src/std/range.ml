open Ord

module Range = struct
  module Inclusive = struct
    type 'a t =
      { min : 'a
      ; max : 'a
      }

    let unite : 'a. 'a Ord.t -> 'a t -> 'a t -> 'a t =
      fun ord a b -> { min = Ord.min ord a.min b.min; max = Ord.max ord a.max b.max }
    ;;

    let point : 'a. 'a -> 'a t = fun x -> { min = x; max = x }
  end

  type 'a inclusive = 'a Inclusive.t
end
