module Lexer where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Control.Monad.State (evalState)
import Control.Monad.State.Lazy (MonadState (get), State, modify)
import Data.Char
import MyPrelude

type Parse a = State ParserState a

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

data SourceFile = SourceFile {filename :: String, contents :: String}
  deriving (Show)

data Position = Position {index :: Int, line :: Int, column :: Int}
  deriving (Show)

data Span = Span {start :: Position, end :: Position, filename :: String}
  deriving (Show)

data Spanned a = Spanned {value :: a, span :: Span}
  deriving (Show)

data ParserState = ParserState {filename :: String, contents :: String, position :: Position}
  deriving (Show)

parse :: SourceFile -> [Spanned Token]
parse SourceFile {..} =
  evalState
    parseImpl
    ParserState
      { filename,
        contents,
        position =
          Position
            { index = 0,
              line = 1,
              column = 1
            }
      }

parseImpl :: Parse [Spanned Token]
parseImpl = do
  skipWhitespace
  parseOneSpanned >>= \case
    Nothing -> return []
    Just token -> do
      rest <- parseImpl
      return $ token : rest

skipWhitespace :: Parse ()
skipWhitespace =
  whileM_
    ( peek
        <&> \case
          Just c | isSpace c -> True
          _ -> False
    )
    consume

currentPosition :: Parse Position
currentPosition = get <&> position

parseOneSpanned :: Parse (Maybe (Spanned Token))
parseOneSpanned = do
  filename <- get <&> \state -> state.filename
  start <- currentPosition
  maybeToken <- parseOne
  end <- currentPosition
  case maybeToken of
    Just token -> return $ Just Spanned {value = token, span = Span {start, end, filename}}
    Nothing -> return Nothing

parseOne :: Parse (Maybe Token)
parseOne =
  peek >>= \case
    Nothing -> return Nothing
    Just '#' -> do
      consume
      comment <- readUntilChar '\n'
      return $ Just (Comment comment)
    Just c
      | isDigit c ->
          Just . Number <$> readUntil (not . isDigit)
    Just '"' -> do
      consume
      value <- readUntilChar '"'
      assertConsume '"'
      return $ Just $ String {raw = '"' : value ++ "\"", value}
    Just c
      | isAlpha c || c == '_' ->
          Just . Ident <$> readUntil (\c' -> not (isAlphaNum c' || c' == '_'))
    Just c -> do
      consume
      return $ Just (Punctuation [c])

peek :: Parse (Maybe Char)
peek = do
  state <- get
  return $ head state.contents

assertConsume :: Char -> Parse ()
assertConsume expected = do
  actual <- next
  when
    (actual /= Just expected)
    ( error $ "expected " ++ show expected ++ ", got " ++ show actual
    )

consume :: Parse ()
consume =
  modify $ \state -> case state.contents of
    "" -> error "No character when consuming"
    '\n' : rest ->
      state
        { contents = rest,
          position =
            Position
              { line = state.position.line + 1,
                column = 1,
                index = state.position.index + 1
              }
        }
    _ : rest -> do
      state
        { contents = rest,
          position =
            Position
              { line = state.position.line,
                column = state.position.column + 1,
                index = state.position.index + 1
              }
        }

next :: Parse (Maybe Char)
next = do
  c <- peek
  case c of
    Just _ -> consume
    Nothing -> return ()
  return c

readUntilChar :: Char -> Parse String
readUntilChar c = do
  readUntil (== c)

-- | read until the predicate is true
readUntil :: (Char -> Bool) -> Parse String
readUntil f =
  peek >>= \case
    Nothing -> return ""
    Just c | f c -> return ""
    Just c -> do
      consume
      rest <- readUntil f
      return $ c : rest
