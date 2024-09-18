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

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

readIdent :: (Reading :> es, Fail :> es) => SpecificReader es Token
readIdent peeked = toMaybe (isIdentStart peeked) do
  name <- readWhile \c -> isIdentStart c || isDigit c
  return $ Ident{raw = name, name, isRaw = False}

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
      String{value} -> value
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
