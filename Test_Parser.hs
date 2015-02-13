module Main (main) where
import System.Environment
import Abs
import Scanner
import Parser

main = do
  [arg] <- getArgs
  src <- readFile arg
  putStrLn(drawAst(parseJava(alexScanTokens src)))
