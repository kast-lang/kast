module Lexer where

import Control.Monad
import Data.Char hiding (isPunctuation)
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Effectful
import Reader
import Prelude

data Token
  = Ident {raw :: String, name :: String, isRaw :: Bool}
  | String {raw :: String, value :: String}
  | Punctuation String
  | Number String
  | Comment String
  deriving (Show)

rawToken :: Token -> String
rawToken = \case
  Ident{raw} -> raw
  String{raw} -> raw
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
    Just token -> return $ Just SpannedToken{token, span = Span{start, end, filename}}
    Nothing -> return Nothing

parseOne :: (Reading :> es) => Eff es (Maybe Token)
parseOne =
  peek >>= \case
    Nothing -> return Nothing
    Just peeked ->
      Just <$> case peeked of
        '#' -> readComment
        c | isDigit c -> readNumber
        '"' -> readString
        '@' -> readRawIdent
        c | isIdentStart c -> readIdent
        c | isPunctuation c -> readPunctuation
        c -> error $ "Unexpected character: " ++ show c
 where
  readComment = do
    skipChar '#'
    comment <- readUntilChar '\n'
    return $ Comment comment
  readNumber = do
    s <- readWhile \c -> isDigit c || c == '.' || c == '_'
    return $ Number s
  readString = do
    rawRecording <- startRecording
    skipChar '"'
    value <- readUntilChar '"'
    skipChar '"'
    raw <- stopRecording rawRecording
    return $ String{raw, value}
  isIdentStart c = isAlpha c || c == '_'
  readIdent = do
    name <- readWhile \c -> isIdentStart c || isDigit c
    return $ Ident{raw = name, name, isRaw = False}
  readRawIdent = do
    rawRecording <- startRecording
    skipChar '@'
    name <-
      readString <&> \case
        String{value} -> value
        _ -> error "reading string didnt result in string???"
    raw <- stopRecording rawRecording
    return $ Ident{raw, name, isRaw = True}
  readPunctuation = do
    s <-
      peek >>= \case
        Just c | isSinglePunctuation c -> do
          skipChar c
          return [c]
        _ -> readWhile (\c -> isPunctuation c && not (isSinglePunctuation c))
    return $ Punctuation s
  isPunctuation c = not (isAlphaNum c || isSpace c || c == '\'' || c == '"')
  isSinglePunctuation c = isJust $ find (== c) "(){}[]"
