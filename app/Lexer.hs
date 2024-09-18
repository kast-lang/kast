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
import Utils
import Prelude

data StringType = SingleQuoted | DoubleQuoted
  deriving (Show)

quoteChar :: StringType -> Char
quoteChar = \case
  SingleQuoted -> '\''
  DoubleQuoted -> '"'

data Token
  = Ident {raw :: String, name :: String, isRaw :: Bool}
  | String {raw :: String, contents :: String, type_ :: StringType}
  | Punctuation {raw :: String}
  | Number {raw :: String}
  | Comment {raw :: String}
  deriving (Show)

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
  maybeParseOneSpanned >>= \case
    Nothing -> return []
    Just token -> do
      rest <- parseImpl
      return $ token : rest

maybeParseOneSpanned :: (Reading :> es, Fail :> es) => Eff es (Maybe SpannedToken)
maybeParseOneSpanned = do
  filename <- currentFile
  start <- currentPosition
  maybeToken <- maybeParseOne
  end <- currentPosition
  case maybeToken of
    Just token -> return $ Just SpannedToken{token, span = Span{start, end, filename}}
    Nothing -> return Nothing

maybeParseOne :: (Reading :> es, Fail :> es) => Eff es (Maybe Token)
maybeParseOne =
  peek >>= \case
    Nothing -> return Nothing
    Just _ -> Just <$> parseOne

parseOne :: (Reading :> es, Fail :> es) => Eff es Token
parseOne =
  parseOneOf
    [ readComment
    , readNumber
    , readString
    , readRawIdent
    , readIdent
    , readPunctuation
    ]

type SpecificReader es a = Char -> Maybe (Eff es a)

parseOneOf :: (Reading :> es, Fail :> es) => [SpecificReader es a] -> Eff es a
parseOneOf options = do
  peeked <- fromJust <$> peek
  findMap (\option -> option peeked) options & \case
    Nothing -> fail $ "Unexpected character: " ++ show peeked
    Just option -> option

readComment :: (Reading :> es, Fail :> es) => SpecificReader es Token
readComment peeked = toMaybe (peeked == '#') do
  skipChar '#'
  comment <- readUntilChar '\n'
  return $ Comment comment

readNumber :: (Reading :> es, Fail :> es) => SpecificReader es Token
readNumber peeked = toMaybe (isDigit peeked) do
  s <- readWhile \c -> isDigit c || c == '.' || c == '_'
  return $ Number s

readString :: (Reading :> es, Fail :> es) => SpecificReader es Token
readString peeked =
  findMap
    (\type_ -> toMaybe (quoteChar type_ == peeked) (readType type_))
    [ SingleQuoted
    , DoubleQuoted
    ]
 where
  readType type_ = do
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
                readChar
                  >>= \case
                    Nothing -> fail "Unexpected EOF, expected escaped character in string literal"
                    Just c -> return c
              rest <- readContents
              return ('\\' : c : rest)
            Just c -> do
              skipChar c
              rest <- readContents
              return (c : rest)
    contents <- readContents
    raw <- stopRecording rawRecording
    return $ String{raw, contents, type_}

readIdent :: (Reading :> es, Fail :> es) => SpecificReader es Token
readIdent peeked' = toMaybe (isAlpha peeked' || peeked' == '_') do
  name <- readImpl
  return $ Ident{raw = name, name, isRaw = False}
 where
  readImpl = do
    peeked <- peek
    peeked2 <- peek2
    case (peeked, peeked2) of
      (Just c, c2) | isAlphaNum c || c == '_' || (c == '-' && maybe False isAlphaNum c2) -> do
        skipChar c
        rest <- readImpl
        return (c : rest)
      _ -> return ""

tryRead :: (Reading :> es, Fail :> es) => String -> SpecificReader es a -> Eff es a
tryRead errorMsg reader = do
  peeked <-
    peek >>= \case
      Just c -> return c
      Nothing -> fail errorMsg
  case reader peeked of
    Just r -> r
    Nothing -> fail errorMsg

readRawIdent :: (Reading :> es, Fail :> es) => SpecificReader es Token
readRawIdent peeked = toMaybe (peeked == '@') do
  rawRecording <- startRecording
  skipChar '@'
  name <-
    tryRead "expected a string for a raw token" readString <&> \case
      String{contents} -> contents
      _ -> error "reading string didnt result in string???"
  raw <- stopRecording rawRecording
  return $ Ident{raw, name, isRaw = True}

readPunctuation :: (Reading :> es, Fail :> es) => SpecificReader es Token
readPunctuation peeked = toMaybe (isPunctuation peeked) do
  s <-
    peek >>= \case
      Just c | isSinglePunctuation c -> do
        skipChar c
        return [c]
      _ -> readWhile (\c -> isPunctuation c && not (isSinglePunctuation c))
  return $ Punctuation s

isPunctuation :: Char -> Bool
isPunctuation c = not (isAlphaNum c || isSpace c || c == '\'' || c == '"')

isSinglePunctuation :: Char -> Bool
isSinglePunctuation c = isJust $ find (== c) "(){}[]"
