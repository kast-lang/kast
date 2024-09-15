module Main where

import Lexer qualified
import MyPrelude
import System.IO
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  contents <- getContents
  let tokens = Lexer.parse Lexer.SourceFile {filename = "<stdin>", contents}
   in pPrint tokens
