module Main where

import Lexer qualified
import MyPrelude
import System.IO

main :: IO ()
main = do
  contents <- getContents
  let tokens = Lexer.parse Lexer.SourceFile {filename = "<stdin>", contents}
   in print tokens
