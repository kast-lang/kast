module Syntax (
  Associativity (..),
  emptySyntax,
  addSyntax,
  Syntax (..),
  Edge (..),
  ParseStateNode (..),
  SyntaxDefinition (..),
  SyntaxDefinitionPart (..),
  BindingPower (..),
) where

import Data.Map
import Data.Maybe
import Data.Set
import Prelude hiding (Left)

data Associativity = Left | Right
  deriving (Show, Eq)

data SyntaxDefinitionPart
  = Keyword String
  | Named String
  | Unnamed
  deriving (Show)

data SyntaxDefinition = SyntaxDefinition
  { name :: String
  , associativity :: Associativity
  , priority :: Float
  , parts :: [SyntaxDefinitionPart]
  }
  deriving (Show)

data Edge = Edge
  { keyword :: String
  , values :: Int
  }
  deriving (Show, Ord, Eq)

data BindingPower = BindingPower
  { priority :: Float
  , associativity :: Associativity
  }
  deriving (Show, Eq)

data ParseStateNode = ParseStateNode
  { finish :: Map Int SyntaxDefinition
  , next :: Map Edge ParseStateNode
  , bindingPower :: BindingPower
  }
  deriving (Show)

emptyParseStateNode :: BindingPower -> ParseStateNode
emptyParseStateNode power =
  ParseStateNode
    { finish = Data.Map.empty
    , next = Data.Map.empty
    , bindingPower = power
    }

data Syntax = Syntax
  { keywords :: Set String
  , rootStateNode :: ParseStateNode
  }
  deriving (Show)

emptySyntax :: Syntax
emptySyntax =
  Syntax
    { keywords = Data.Set.empty
    , rootStateNode =
        emptyParseStateNode
          BindingPower
            { priority = 0
            , associativity = Left
            }
    }

addSyntax :: Syntax -> SyntaxDefinition -> Syntax
addSyntax syntax def =
  Syntax
    { keywords = updateKeywords syntax.keywords def.parts
    , rootStateNode = insertDef def syntax.rootStateNode def.parts
    }

updateKeywords :: Set String -> [SyntaxDefinitionPart] -> Set String
updateKeywords keywords = \case
  [] -> keywords
  Keyword keyword : rest ->
    updateKeywords (Data.Set.insert keyword keywords) rest
  _ : rest ->
    updateKeywords keywords rest

insertDef :: SyntaxDefinition -> ParseStateNode -> [SyntaxDefinitionPart] -> ParseStateNode
insertDef def = insertDefImpl 0
 where
  insertDefImpl :: Int -> ParseStateNode -> [SyntaxDefinitionPart] -> ParseStateNode
  insertDefImpl values state = \case
    [] ->
      ParseStateNode
        { finish =
            Data.Map.alter
              ( \case
                  Just existing ->
                    error $
                      "duplicate syntax definition: "
                        ++ show existing.name
                        ++ " and "
                        ++ show def.name
                  Nothing -> Just def
              )
              values
              state.finish
        , next = state.next
        , bindingPower = state.bindingPower
        }
    Keyword keyword : rest ->
      let edge = Edge{keyword, values}
       in ParseStateNode
            { finish = state.finish
            , next =
                Data.Map.alter
                  ( \current ->
                      let power =
                            BindingPower
                              { priority = def.priority
                              , associativity = Left
                              }
                       in Just $
                            insertDefImpl
                              0
                              ( case current of
                                  Nothing -> emptyParseStateNode power
                                  Just current' ->
                                    if current'.bindingPower == power
                                      then current'
                                      else error "different binding power"
                              )
                              rest
                  )
                  edge
                  state.next
            , bindingPower = state.bindingPower
            }
    _ : rest -> insertDefImpl (values + 1) state rest