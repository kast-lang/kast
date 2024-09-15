module MyPrelude
  ( Char,
    IO,
    Int,
    Bool (..),
    Maybe (..),
    String,
    ($),
    (+),
    (==),
    (.),
    (||),
    (&&),
    not,
    error,
    todo,
    head,
    fmap,
    Show,
  )
where

import Prelude
  ( Bool (..),
    Char,
    IO,
    Int,
    Maybe (..),
    Show,
    String,
    error,
    fmap,
    not,
    ($),
    (&&),
    (+),
    (.),
    (==),
    (||),
  )

todo :: a
todo = error "todo"

head :: [a] -> Maybe a
head list = case list of
  x : _ -> Just x
  [] -> Nothing
