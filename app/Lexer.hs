module Lexer where

import Control.Monad
import Data.Char
import Effectful
import MyPrelude
import Reader

data Token
  = Ident String
  | String {raw :: String, value :: String}
  | Punctuation String
  | Number String
  | Comment String
  deriving (Show)

rawToken :: Token -> String
rawToken = \case
  Ident raw -> raw
  String {raw} -> raw
  Punctuation raw -> raw
  Number raw -> raw
  Comment raw -> raw

data Span = Span {start :: Position, end :: Position, filename :: String}
  deriving (Show)

data SpannedToken = SpannedToken {token :: Token, span :: Span}
  deriving (Show)

data ParserState = ParserState {filename :: String, contents :: String, position :: Position}
  deriving (Show)

parse :: SourceFile -> Eff es [SpannedToken]
parse sourceFile =
  Reader.read
    sourceFile
    parseImpl

parseImpl :: (Reading :> es) => Eff es [SpannedToken]
parseImpl = do
  skipWhitespace
  parseOneSpanned >>= \case
    Nothing -> return []
    Just token -> do
      rest <- parseImpl
      return $ token : rest

parseOneSpanned :: (Reading :> es) => Eff es (Maybe SpannedToken)
parseOneSpanned = do
  filename <- currentFile
  start <- currentPosition
  maybeToken <- parseOne
  end <- currentPosition
  case maybeToken of
    Just token -> return $ Just SpannedToken {token, span = Span {start, end, filename}}
    Nothing -> return Nothing

parseOne :: (Reading :> es) => Eff es (Maybe Token)
parseOne =
  peek >>= \case
    Nothing -> return Nothing
    Just '#' -> do
      skipChar '#'
      comment <- readUntilChar '\n'
      return $ Just (Comment comment)
    Just c
      | isDigit c ->
          Just . Number <$> readWhile isDigit
    Just '"' -> do
      skipChar '"'
      value <- readUntilChar '"'
      skipChar '"'
      return $ Just $ String {raw = '"' : value ++ "\"", value}
    Just c
      | isAlpha c || c == '_' ->
          Just . Ident <$> readUntil (\c' -> not (isAlphaNum c' || c' == '_'))
    Just c -> do
      skipChar c
      return $ Just (Punctuation [c])
