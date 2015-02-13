module Main (main) where
import System.Environment
import Scanner
import Abs
import Parser
import Types
import Translation

main = do
  [arg] <- getArgs
  src <- readFile arg
  putStrLn (show (ap_getCode(translateC(head(typeCheck (parseJava(alexScanTokens src)))))))
  putStrLn (show (ap_getMethodPool(translateC(head(typeCheck (parseJava(alexScanTokens src)))))))
  -- putStrLn (drawAst (parseJava(alexScanTokens "public class t {void test () {a = 0x123; } }")))
