module Lexer where

import Control.Monad
import Control.Monad.Loops (whileM_)
import Data.Char
import Effectful
import Effectful.State.Static.Shared
import MyPrelude

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
  String{raw} -> raw
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
parse SourceFile{..} =
  runPureEff
    ( evalState
        ParserState
          { filename
          , contents
          , position =
              Position
                { index = 0
                , line = 1
                , column = 1
                }
          }
        ( parseImpl
            @'[State ParserState]
        )
    )

parseImpl :: (State ParserState :> es) => Eff es [Spanned Token]
parseImpl = do
  skipWhitespace
  parseOneSpanned >>= \case
    Nothing -> return []
    Just token -> do
      rest <- parseImpl
      return $ token : rest

skipWhitespace :: (State ParserState :> es) => Eff es ()
skipWhitespace =
  whileM_
    ( peek
        <&> \case
          Just c | isSpace c -> True
          _ -> False
    )
    consume

currentPosition :: (State ParserState :> es) => Eff es Position
currentPosition = get <&> position

parseOneSpanned :: (State ParserState :> es) => Eff es (Maybe (Spanned Token))
parseOneSpanned = do
  filename <- get @ParserState <&> \state -> state.filename
  start <- currentPosition
  maybeToken <- parseOne
  end <- currentPosition
  case maybeToken of
    Just token -> return $ Just Spanned{value = token, span = Span{start, end, filename}}
    Nothing -> return Nothing

parseOne :: (State ParserState :> es) => Eff es (Maybe Token)
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
      return $ Just $ String{raw = '"' : value ++ "\"", value}
    Just c
      | isAlpha c || c == '_' ->
          Just . Ident <$> readUntil (\c' -> not (isAlphaNum c' || c' == '_'))
    Just c -> do
      consume
      return $ Just (Punctuation [c])

peek :: (State ParserState :> es) => Eff es (Maybe Char)
peek = do
  state <- get @ParserState
  return $ head state.contents

assertConsume :: (State ParserState :> es) => Char -> Eff es ()
assertConsume expected = do
  actual <- next
  when
    (actual /= Just expected)
    ( error $ "expected " ++ show expected ++ ", got " ++ show actual
    )

consume :: (State ParserState :> es) => Eff es ()
consume =
  modify @ParserState $ \state -> case state.contents of
    "" -> error "No character when consuming"
    '\n' : rest ->
      state
        { contents = rest
        , position =
            Position
              { line = state.position.line + 1
              , column = 1
              , index = state.position.index + 1
              }
        }
    _ : rest -> do
      state
        { contents = rest
        , position =
            Position
              { line = state.position.line
              , column = state.position.column + 1
              , index = state.position.index + 1
              }
        }

next :: (State ParserState :> es) => Eff es (Maybe Char)
next = do
  c <- peek
  case c of
    Just _ -> consume
    Nothing -> return ()
  return c

readUntilChar :: (State ParserState :> es) => Char -> Eff es String
readUntilChar c = do
  readUntil (== c)

-- | read until the predicate is true
readUntil :: (State ParserState :> es) => (Char -> Bool) -> Eff es String
readUntil f =
  peek >>= \case
    Nothing -> return ""
    Just c | f c -> return ""
    Just c -> do
      consume
      rest <- readUntil f
      return $ c : rest
