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
      '.'			{DotToken _}
      boolean			{BooleanToken _}
      integer			{IntToken _}
      char 			{CharToken _}
      '('			{LeftParenthesisToken _}
      ')'			{RightParenthesisToken _}
      void			{VoidToken _}
      ','			{CommaToken _}

%%

classdef :: {ClassDef}
classdef 		: class identifier classbody			{ ClassDef($2, [], [], [], []) }
			| modifiers class identifier classbody		{ ClassDef($3, $1, fst $4, [], [])}



modifiers 		: modifiers modifier 				{$1 ++ [$2]}
			| modifier					{[$1]}
								
modifier		: public 					{Public}
			| private 					{Private}
			| final 					{Final}
			| static 					{Static}

classbody 		: '{' classbodyelements '}'		{$2}
			| '{' '}'				{([],[])}
				
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
					
classbodyelement 	: functiondecl  {Function $1}
			-- | fielddecl  {Field $1}
					

functiondecl		: functionhead				{MemberFunction(fst $1, fst (snd $1), fst (snd (snd $1)), snd (snd (snd $1)), Block([]))}

functionhead		: functiontype identifier functionparameters	{($1,($2,($3,[])))}
			| modifiers functiontype identifier functionparameters	{($2,($3,($4,[])))}

functionparameters	: '(' formalparameters ')'			{$2}
			| '(' ')'					{[]}

formalparameters	: formalparameters ',' formalparameter		{$1 ++ [$3]}
			| formalparameter				{[$1]}
			
formalparameter		: type identifier				{($1, $2)}

name 			: qualifiedname		{$1}
			| simplename		{$1}
					
simplename		: identifier		{$1}

qualifiedname		: name '.' identifier	{$1 ++ "." ++ $3}

type 			: primitivetype			{$1}
			| referencetype			{$1}
					
primitivetype		: boolean		{"boolean"}
			| integer		{"int"}
			| char 			{"char"}
					
referencetype		: name			{$1}


functiontype		: type			{$1}
			| void			{"void"}

{

data ClassElement a
	= Field a
	| Function a
	| Constructor a






parseError :: [Token] -> a
parseError t = error (show t)

main = do
    putStrLn (drawAst(parseJava(alexScanTokens "public static class test { void t (int g, int h) }")))
}
