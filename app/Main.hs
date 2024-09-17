module Main where

import Effectful
import Lexer qualified
import MyPrelude
import Reader qualified
import System.IO
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  contents <- getContents
  let tokens = runPureEff $ Lexer.parse Reader.SourceFile {filename = "<stdin>", contents}
  pPrint tokens
