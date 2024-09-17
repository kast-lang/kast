module Syntax where

import Data.Set
import MyPrelude

data Associativity = Left | Right
  deriving (Show)

data SyntaxDefinitionPart
  = Keyword String
  | Named String
  | Unnamed
  deriving (Show)

data SyntaxDefinition = SyntaxDef
  { name :: String
  , associativity :: Associativity
  , priority :: Float
  , parts :: [SyntaxDefinitionPart]
  }
  deriving (Show)

data Syntax = Syntax
  { keywords :: Set String
  }
  deriving (Show)
