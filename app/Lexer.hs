module Lexer where

import Control.Monad
import Data.Char hiding (isPunctuation)
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import Effectful
import Effectful.Fail
import Reader
import Prelude

data StringType = SingleQuoted | DoubleQuoted
  deriving (Show)

data Token
  = Ident {raw :: String, name :: String, isRaw :: Bool}
  | String {raw :: String, value :: String, type_ :: StringType}
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

parse :: (Fail :> es) => SourceFile -> Eff es [SpannedToken]
parse sourceFile =
  inject $
    Reader.read
      sourceFile
      parseImpl

parseImpl :: (Reading :> es, Fail :> es) => Eff es [SpannedToken]
parseImpl = do
  skipWhitespace
  parseOneSpanned >>= \case
    Nothing -> return []
    Just token -> do
      rest <- parseImpl
      return $ token : rest

parseOneSpanned :: (Reading :> es, Fail :> es) => Eff es (Maybe SpannedToken)
parseOneSpanned = do
  filename <- currentFile
  start <- currentPosition
  maybeToken <- parseOne
  end <- currentPosition
  case maybeToken of
    Just token -> return $ Just SpannedToken{token, span = Span{start, end, filename}}
    Nothing -> return Nothing

tryRead :: (Reading :> es) => Eff (Fail : es) a -> Eff es (Either String a)
tryRead f = do
  originalState <- saveState
  runFail f >>= \case
    Right value -> return $ Right value
    Left e -> do
      resetState originalState
      return $ Left e

parseOne :: (Reading :> es, Fail :> es) => Eff es (Maybe Token)
parseOne =
  peek >>= \case
    Nothing -> return Nothing
    Just peeked ->
      Just <$> case peeked of
        '#' -> readComment
        c | isDigit c -> readNumber
        '\'' -> readString SingleQuoted
        '"' -> readString DoubleQuoted
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
  readString type_ = do
    start <- currentPosition
    let quote = case type_ of
          SingleQuoted -> '\''
          DoubleQuoted -> '"'
    rawRecording <- startRecording
    skipChar quote
    let readContents =
          peek >>= \case
            Nothing -> fail $ "Unexpected EOF, string starting at " ++ showLineColumn start ++ " is not terminated"
            Just c | c == quote -> do
              skipChar quote
              return ""
            Just '\\' -> do
              skipChar '\\'
              c <-
                peek >>= \case
                  Nothing -> fail "Unexpected EOF, expected escaped character in string literal"
                  Just '\\' -> fromJust <$> readChar
                  Just '\'' -> fromJust <$> readChar
                  Just '"' -> fromJust <$> readChar
                  Just 'x' -> do
                    skipChar 'x'
                    let readHexDigit =
                          peek >>= \case
                            Just c | isHexDigit c -> do
                              skipChar c
                              return $ digitToInt c
                            _ -> fail "expected a hex digit"
                    first <- readHexDigit
                    second <- readHexDigit
                    let hexCode = first * 16 + second
                    return $ chr hexCode
                  Just '0' -> do
                    skipChar '0'
                    return '\0'
                  Just 'n' -> do
                    skipChar 'n'
                    return '\n'
                  Just 'r' -> do
                    skipChar 'r'
                    return '\r'
                  Just 't' -> do
                    skipChar 't'
                    return '\t'
                  Just _ -> fail "Unexpected escape character"
              rest <- readContents
              return (c : rest)
            Just c -> do
              skipChar c
              rest <- readContents
              return (c : rest)
    value <- readContents
    raw <- stopRecording rawRecording
    return $ String{raw, value, type_}
  isIdentStart c = isAlpha c || c == '_'
  readIdent = do
    name <- readWhile \c -> isIdentStart c || isDigit c
    return $ Ident{raw = name, name, isRaw = False}
  readRawIdent = do
    rawRecording <- startRecording
    skipChar '@'
    name <-
      readString DoubleQuoted <&> \case
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
