module Main (main, parseJava) where
import System.Environment
import Scanner
import Abs
import Parser

main = do
  [arg] <- getArgs
  src <- readFile arg
  putStrLn (drawAst(parseJava(alexScanTokens src)))
  -- putStrLn (drawAst(parseJava(alexScanTokens "public class t {void test () {
  -- for(;;) {int a = 5;} }}")))
