module Reader
  ( Reading,
    Position,
    SourceFile (..),
    read,
    currentPosition,
    currentLine,
    currentFile,
    peek,
    skipAnyChar,
    skipWhile,
    skipWhitespace,
    readChar,
    skipChar,
    readWhile,
    readUntil,
    readUntilChar,
  )
where

import Control.Monad.Loops (whileM)
import Data.Char
import Data.Functor ((<&>))
import Data.Maybe (fromJust, listToMaybe)
import Effectful
import Effectful.State.Static.Shared
import Utils
import Prelude hiding (read)

type Reading = State ReaderState

data Position = Position {index :: Int, line :: Int, column :: Int}
  deriving (Show)

data ReaderState = ReaderState
  { filename :: String,
    remaining_contents :: String,
    position :: Position
  }

data SourceFile = SourceFile {filename :: String, contents :: String}
  deriving (Show)

read :: SourceFile -> Eff (Reading : es) a -> Eff es a
read SourceFile {..} =
  evalState
    ReaderState
      { filename,
        remaining_contents = contents,
        position =
          Position
            { index = 0,
              line = 1,
              column = 1
            }
      }

currentPosition :: (Reading :> es) => Eff es Position
currentPosition = get <&> position

currentLine :: (Reading :> es) => Eff es Int
currentLine = currentPosition <&> line

currentFile :: (Reading :> es) => Eff es String
currentFile = get @ReaderState <&> \reader -> reader.filename

peek :: (Reading :> es) => Eff es (Maybe Char)
peek = get <&> listToMaybe . remaining_contents

skipWhile :: (Reading :> es) => (Char -> Bool) -> Eff es ()
skipWhile predicate = readWhile predicate <&> ignore

skipWhitespace :: (Reading :> es) => Eff es ()
skipWhitespace = skipWhile isSpace

readWhile :: (Reading :> es) => (Char -> Bool) -> Eff es String
readWhile predicate = whileM (peek <&> maybe False predicate) (readChar <&> fromJust)

readUntil :: (Reading :> es) => (Char -> Bool) -> Eff es String
readUntil predicate = readWhile (not . predicate)

readUntilChar :: (Reading :> es) => Char -> Eff es String
readUntilChar c = readUntil (== c)

skipAnyChar :: (Reading :> es) => Eff es ()
skipAnyChar =
  readChar <&> \case
    Just _ -> ()
    Nothing -> error "there is nothing to skip"

skipChar :: (Reading :> es) => Char -> Eff es ()
skipChar expected =
  readChar <&> \case
    Just actual | actual == expected -> ()
    Just actual -> error $ "expected " ++ show expected ++ ", got " ++ show actual
    Nothing -> error $ "expected " ++ show expected ++ ", got nothing"

readChar :: (Reading :> es) => Eff es (Maybe Char)
readChar = state \reader ->
  case reader.remaining_contents of
    [] -> (Nothing, reader)
    c : remaining_contents ->
      let Position {..} = reader.position
          newPosition = case c of
            '\n' -> Position {index = index + 1, line = line + 1, column = 1}
            _ -> Position {index = index + 1, line, column = column + 1}
       in (Just c, reader {remaining_contents, position = newPosition})
