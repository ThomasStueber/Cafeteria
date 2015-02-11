module Main (main) where
import System.Environment
import Scanner
import Abs
import Parser
import Types

main = do
  [arg] <- getArgs
  src <- readFile arg
  --putStrLn (drawAst(parseJava(alexScanTokens src)))
  putStrLn (show (typeCheck (parseJava(alexScanTokens src))))
  -- putStrLn (drawAst (parseJava(alexScanTokens "public class t {void test () {a = 0x123; } }")))
