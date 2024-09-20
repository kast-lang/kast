{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast where

import Control.Monad
import Data.Map hiding (splitAt)
import Data.Maybe
import Effectful
import Effectful.State.Static.Shared
import Lexer
import Reader
import Syntax
import Utils
import Prelude

data AstNode d
  = Simple {token :: Token, data_ :: d}
  | Complex
      { definition :: SyntaxDefinition
      , values :: Tuple (AstNode d)
      , data_ :: d
      }
  deriving (Show)

type Ast = AstNode Span

data Tuple a = Tuple
  { unnamed :: [a]
  , named :: Map String a
  }
  deriving (Show)

emptyTuple :: Tuple a
emptyTuple =
  Tuple
    { unnamed = []
    , named = Data.Map.empty
    }

data ParseState = ParseState
  { node :: ParseStateNode
  , startPosition :: Maybe Position
  , currentPosition :: Maybe Position
  , filename :: Maybe String
  , currentNodeValues :: [Ast]
  , unassignedValues :: [Ast]
  , until :: Maybe BindingPower
  }

data FinishResult = FinishResult {value :: Ast, unassignedValues :: [Ast]}

parse :: Syntax -> [SpannedToken] -> Ast
parse syntax =
  parseUntil newState
 where
  newState :: ParseState
  newState =
    ParseState
      { node = syntax.rootStateNode
      , startPosition = Nothing
      , currentPosition = Nothing
      , filename = Nothing
      , currentNodeValues = []
      , unassignedValues = []
      , until = Nothing
      }

  parseUntil :: ParseState -> [SpannedToken] -> Ast
  parseUntil state = \case
    [] ->
      let FinishResult{..} = finish state
       in case unassignedValues of
            [] -> value
            _ -> parseUntil newState{unassignedValues = unassignedValues ++ [value]} []
    token : rest ->
      let edge = Edge{keyword = raw token.token, values = length state.unassignedValues}
          maybeNextNode :: Maybe ParseStateNode = Data.Map.lookup edge state.node.next
       in case maybeNextNode of
            Just nextNode
              | shouldContinue state nextNode ->
                  let nextState =
                        ParseState
                          { node = nextNode
                          , startPosition = Just $ fromMaybe token.span.start state.startPosition
                          , currentPosition = Just token.span.end
                          , filename = Just token.span.filename
                          , currentNodeValues = state.currentNodeValues ++ state.unassignedValues
                          , unassignedValues = []
                          , until = state.until
                          }
                   in parseUntil nextState rest
            _ -> todo

  shouldContinue :: ParseState -> ParseStateNode -> Bool
  shouldContinue state nextNode =
    -- a + b + c
    compare (Just nextNode.bindingPower.priority) ((\power -> power.priority) <$> state.until) |> \case
      GT -> True
      EQ -> maybe True (\power -> power.associativity == Syntax.Right) state.until
      LT -> False

  finish :: ParseState -> FinishResult
  finish state =
    let (amountOfFinalValues, definition) =
          Data.Map.lookupLE (length state.unassignedValues) state.node.finish |> \case
            Just x -> x
            Nothing -> error "can't finish"
        (finalValues, unassignedValues) = splitAt amountOfFinalValues state.unassignedValues
        -- this line is a bug but its ok
        childValues = state.currentNodeValues ++ finalValues
        value =
          Complex
            { definition
            , values = collectValues definition childValues
            , data_ =
                Span
                  { start = fromJust state.startPosition
                  , end = fromJust state.currentPosition
                  , filename = fromJust state.filename
                  }
            }
     in FinishResult{value, unassignedValues}

collectValues :: SyntaxDefinition -> [Ast] -> Tuple Ast
collectValues def = collectValuesImpl emptyTuple def.parts
 where
  collectValuesImpl :: Tuple Ast -> [SyntaxDefinitionPart] -> [Ast] -> Tuple Ast
  collectValuesImpl current = \case
    [] -> \case
      [] -> current
      _ -> error "too many values for syntax definition"
    Keyword _ : rest -> collectValuesImpl current rest
    Unnamed : rest -> \case
      [] -> error "not enough values for syntax definition"
      x : xs -> collectValuesImpl current{unnamed = current.unnamed ++ [x]} rest xs
    Named name : rest -> \case
      [] -> error "not enough values for syntax definition"
      x : xs -> collectValuesImpl current{named = Data.Map.insert name x current.named} rest xs