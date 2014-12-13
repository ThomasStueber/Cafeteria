module Main (main, parseJava) where
import Scanner
import Abs
import Parser



main = do
    putStrLn (drawAst(parseJava(alexScanTokens "public class t {void test () {((this.b.a().c)) = ~true && true;}}")))