module Utils (ignore, toMaybe, findMap) where

import Data.Maybe
import Prelude

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
