# class Functor f where
#     fmap :: (a -> b) -> f a -> f b
#
# instance Functor Maybe where
#   fmap _ Nothing = Nothing
#   fmap f (Just a) = Just (f a)

module:

use std.*;

const Functor = forall[F :: type -> type] {
    .fmap = forall[a :: type, b :: type] {
        (a -> b) -> F a -> F b
    },
};

const option = T => Option[T];

const add-five = forall[F] {
  f => (F as Functor).fmap (x => x + 5)
};

add-five[option] (:Some (123 :: int32))
