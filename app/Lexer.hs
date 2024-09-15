module Lexer where

import Data.Char
import Data.List (length, take)
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

data Source = Source {filename :: String, contents :: String, position :: Position}
  deriving (Show)

parse :: SourceFile -> [Spanned Token]
parse SourceFile {..} = parseImpl (Source {filename, contents, position = Position {index = 0, line = 1, column = 1}})

parseImpl :: Source -> [Spanned Token]
parseImpl source =
  let sourceTail = skipWhitespace source
      (maybeToken, sourceTail') = parseOne sourceTail
   in case maybeToken of
        Just token -> token : parseImpl sourceTail'
        Nothing -> []

skipWhitespace :: Source -> Source
skipWhitespace source = case source.contents of
  c : _sourceTail | isSpace c -> skipWhitespace $ advance source
  _ -> source

advance :: Source -> Source
advance Source {filename, contents, position = Position {..}} = case contents of
  "" -> error "This was not supposed to happen"
  '\n' : sourceTail ->
    Source
      { filename,
        contents = sourceTail,
        position =
          Position
            { index = index + 1,
              line = line + 1,
              column = 1
            }
      }
  _ : sourceTail ->
    Source
      { filename,
        contents = sourceTail,
        position =
          Position
            { index = index + 1,
              line,
              column = column + 1
            }
      }

parseOne :: Source -> (Maybe (Spanned Token), Source)
parseOne source =
  let (token :: Maybe Token, sourceAfter :: Source) = case head source.contents of
        Nothing -> (Nothing, source)
        Just '#' ->
          let (comment, sourceAfterComment) = readUntil '\n' (advance source)
           in ( Just
                  $ Comment
                    comment,
                sourceAfterComment
              )
        Just c
          | isDigit c ->
              let (rawNumber, sourceAfterNumber) = readUntilF (not . isDigit) source
               in (Just $ Number rawNumber, sourceAfterNumber)
        Just '"' ->
          let (value, sourceAfterValue) = readUntil '"' (advance source)
              sourceAfterString = advance sourceAfterValue
           in case head sourceAfterValue.contents of
                Just '"' ->
                  ( Just
                      $ String
                        { raw = take (length value) source.contents,
                          value
                        },
                    sourceAfterString
                  )
                _ -> error "expected closing '\"'"
        Just c
          | isAlpha c || c == '_' ->
              let (ident, sourceAfterIdent) = readUntilF (\c' -> not (isAlphaNum c' || c' == '_')) source
               in (Just $ Ident ident, sourceAfterIdent)
        Just c -> (Just $ Punctuation [c], advance source)
   in ( fmap
          ( \token' ->
              Spanned
                { value = token',
                  span =
                    Span
                      { start = source.position,
                        end = sourceAfter.position,
                        filename = source.filename
                      }
                }
          )
          token,
        sourceAfter
      )

readUntil :: Char -> Source -> (String, Source)
readUntil until = readUntilF (== until)

readUntilF :: (Char -> Bool) -> Source -> (String, Source)
readUntilF until source = case head source.contents of
  Nothing -> ("", source)
  Just c | until c -> ("", source)
  Just c ->
    let (s, sourceTail) = readUntilF until (advance source)
     in (c : s, sourceTail)
