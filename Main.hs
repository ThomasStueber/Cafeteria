module Main (main, parseJava) where
import Scanner
import Abs
import Parser



main = do
  putStrLn (drawAst(parseJava(alexScanTokens "public class t {void test () { for(;;) {int a = 5;} }}")))