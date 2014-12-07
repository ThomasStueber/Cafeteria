{
module Main (main, parseJava) where
import Scanner
import Abs
}

%name parseJava
%tokentype {Token}
%error {parseError}

%token
      class			{Class _}
      '{' 			{LeftBraces _}
      '}'			{RightBraces _}
      identifier		{Identifier $$ _}

%%

ClassDef 		: class identifier '{' '}'			{ ClassDef($2, [], [], []) }


{

parseError :: [Token] -> a
parseError _ = error "parsing failed"

main = do
    print (alexScanTokens "class test { }")
}
