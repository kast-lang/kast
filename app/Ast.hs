module Ast where

import Control.Monad
import Data.Map
import Effectful
import Effectful.State.Dynamic
import Lexer (Span, SpannedToken, Token)
import MyPrelude
import Syntax

data AstNode d
  = Nothing {data_ :: d}
  | Simple {token :: Token, data_ :: d}
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

class SourceStateMonad m

data SourceState = SourceState
  { filename :: String
  , token :: [SpannedToken]
  }

-- what do I think?
-- parse :: () with State (Syntax | SourceState | Int) -> ()
-- parse :: (Member (State Syntax) r, Member (State SourceState) r, Member (State Int) r) => Eff r ()
parse :: (State Syntax :> es, State SourceState :> es) => Eff es ()
parse = do
  syntax :: Syntax <- get
  sourceState :: SourceState <- get
  return ()
