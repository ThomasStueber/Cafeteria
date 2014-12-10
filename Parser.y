{
module Main (main, parseJava) where
import Scanner
import Abs
import Data.Maybe
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
      '.'				{DotToken _}
      boolean			{BooleanToken _}
      integer			{IntToken _}
      char 				{CharToken _}

%%

classdef 		: class identifier classbody			{ ClassDef($2, [], [], [], []) }
			| modifiers class identifier classbody		{ ClassDef($3, $1, fst $4, [], [])}


modifiers 		: modifiers modifier 				{$1 ++ [$2]}
			| modifier					{[$1]}
								
modifier		: public 					{Public}
			| private 					{Private}
			| final 					{Final}
			| static 					
			{Static}

classbody 		: '{' classbodyelements '}'		{$2}
				| '{' '}'						{([],[])}
				
classbodyelements 	: classbodyelements classbodyelement	
					{
					case $2 of 
							Field f -> (fst $1, (snd $1) ++ [f])
							Function f -> ((fst $1) ++ [f],snd $1)
					} 
					| classbodyelement			
					{
					case $1 of 
							Field f -> ([],[f])
							Function f -> ([f],[])
					}
					
classbodyelement 	: static {Function (MemberFunction("","",[],[],Block([])))}
					-- | functiondecl  {Function $1}
					-- | fielddecl  {Field $1}
					
					

name 				: qualifiedname		{$1}
					| simplename		{$1}
					
simplename			: identifier		{$1}

qualifiedname		: name '.' identifier	{$1 ++ "." ++ $3}

type 				: primitivetype			{$1}
					| referencetype			{$1}
					
primitivetype		: boolean				{"boolean"}
					| integer				{"int"}
					| char 					{"char"}
					
referencetype		: name					{$1}


{

data FieldOrFunction a
	= Field a
	| Function a







parseError :: [Token] -> a
parseError t = error (show t)

main = do
    putStrLn (drawAst(parseJava(alexScanTokens "public static class test { static }")))
}
