module Utils (ignore, toMaybe, findMap, todo, (|>), (<|)) where

import Data.Maybe
import Prelude

todo :: a
todo = error "todo"

ignore :: a -> ()
ignore _ = ()

toMaybe :: Bool -> a -> Maybe a
toMaybe cond value = if cond then Just value else Nothing

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = \case
  [] -> Nothing
  x : xs -> case f x of
    Just result -> Just result
    Nothing -> findMap f xs

infixr 1 |>
(|>) :: a -> (a -> b) -> b
args |> f = f args

infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| args = f args