module Main (main) where
import Scanner
import Abs
import Parser



main = do
  putStrLn (drawAst (parseJava(alexScanTokens "public class t {void test () {this.a.b = 3;}}")))