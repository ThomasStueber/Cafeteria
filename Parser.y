{
module Main (main, parseJava) where
import Scanner
import Abs
}

%name parseJava
%tokentype {Token}
%error {parseError}

%token
      class			{ClassToken _}
      '{' 			{LeftBracesToken _}
      '}'			{RightBracesToken _}
      identifier		{IdentifierToken $$ _}
      public 			{PublicToken _}
      private			{PrivateToken _}
      final 			{FinalToken _}
      static 			{StaticToken _}

%%

classdef 		: class identifier '{' '}'			{ ClassDef($2, [], [], [], []) }
			| modifiers class identifier '{' '}'		{ ClassDef($3, $1, [], [], [])}


modifiers 		: modifiers modifier 				{$1 ++ [$2]}
			| modifier					{[$1]}
								
modifier		: public 					{Public}
			| private 					{Private}
			| final 					{Final}
			| static 					{Static}

{

parseError :: [Token] -> a
parseError t = error (show t)

main = do
    putStrLn (drawAst(parseJava(alexScanTokens "public static class test { }")))
}
