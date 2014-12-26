module Main (main) where
import Scanner
import Abs
import Parser



main = do
  putStrLn (drawAst (parseJava(alexScanTokens "public class t {void test () {while(true) {if (false ) {break;} else break;}}}")))