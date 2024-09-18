module Main where

import Data.Either
import Data.Function
import Effectful
import Effectful.Fail
import Lexer qualified
import MyPrelude
import Reader qualified
import System.IO
import Text.Pretty.Simple (pPrint)

unwrap :: Either String a -> a
unwrap = \case
  Left err -> error err
  Right x -> x

main :: IO ()
main = do
  contents <- getContents
  let tokens =
        runPureEff (runFail (Lexer.parse Reader.SourceFile{filename = "<stdin>", contents}))
          & unwrap
  pPrint tokens
