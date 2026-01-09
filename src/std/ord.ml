module Ord = struct
  type 'a ord = 'a -> 'a -> int
  type 'a t = 'a ord

  let min : 'a. 'a ord -> 'a -> 'a -> 'a = fun ord a b -> if ord a b < 0 then a else b
  let max : 'a. 'a ord -> 'a -> 'a -> 'a = fun ord a b -> if ord b a < 0 then a else b
end
