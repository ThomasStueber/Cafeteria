module Main (main) where
import Scanner
import Abs
import Parser



main = do
  putStrLn (drawAst (parseJava(alexScanTokens "public class t {void test () {a = 077; } }")))