{
module Main (main, parseJava) where
import Scanner
import Abs
import Data.Maybe
}

%name parseJava
%tokentype {Token}
%error {parseError}


-- 1 shift/reduce Konflikt durch if-else Konstrukt, kein Problem da shift gewünschte Operation ist
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
      null			{NullToken _}
      boolean			{BooleanToken _}
      integer			{IntToken _}
      char 			{CharToken _}
      '('			{LeftParenthesisToken _}
      ')'			{RightParenthesisToken _}
      void			{VoidToken _}
      ','			{CommaToken _}
      '+'			{PlusToken _}
      '/'			{DivideToken _}
      '*'			{MulToken _}
      '-'			{MinusToken _}
      '='			{AssignmentToken _}
      '+='			{PlusAssignmentToken _}
      '-='			{MinusAssignmentToken _}
      '*='			{MulAssignmentToken _}
      '/='			{DivideAssignmentToken _}
      '%='			{ModuloAssignmentToken _}
      '<<='			{LShiftAssignmentToken _}
      '>>='			{RShiftAssignmentToken _}
      '>>>='			{UnsignedRShiftAssignmentToken _}
      '^='			{XorAssignmentToken _}
      '|='			{OrAssignmentToken _}
      '&='			{AndAssignmentToken _}
      '||'			{ConditionalOrToken _}
      '&&'			{ConditionalAndToken _}
      '|'			{BitOrToken _}
      '^'			{BitXorToken _}
      '&'			{BitAndToken _}
      '=='			{EqualToken _}
      '!='			{NotEqualToken _}
      '>='			{LessEqualToken _}
      '<='			{GreaterEqualToken _}
      '<'			{GreaterThanToken _}
      '>'			{LessThanToken _}
      '>>'			{SignedRightShiftToken _}
      '<<'			{SignedLeftShiftToken _}
      '>>>'			{UnsignedRightShiftToken _}
      '%'			{ModuloToken _}
      '['			{LeftBracketToken _}
      ']'			{RightBracketToken _}
      '++'			{PlusPlusToken _}
      '--'			{MinusMinusToken _}
      '!'			{NotToken _}
      ';'			{SemicolonToken _}
      literal_int		{IntLiteralToken $$ _}	
      stringliteral		{StringLiteralToken $$ _}
      boolliteral		{BooleanLiteralToken $$ _}
      if 			{IfToken _}
      else 			{ElseToken _}
      return 			{ReturnToken _}
      while 			{WhileToken _}
      do 			{DoToken _}
      new 			{NewToken _}
      
      
      
%right '=' '+=' '-=' '*=' '/=' '%=' '<<=' '>>=' '>>>=' '^=' '|=' '&='
%left '||'
%left '&&'      
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '>=' '<=' '>' '<'
%left '>>' '<<' '>>>' 
%left '+' '-'
%left '*' '/' '%'
%right UNARY
%left '[' ']' '(' ')' '.'
      
%%

classdef :: {ClassDef}
classdef 		: class identifier classbody			{ ClassDef($2, [], [], [], []) }
			| modifiers class identifier classbody		{ ClassDef($3, $1, fst $4, [], [])}


modifiers :: {[Modifier]}
modifiers 		: modifiers modifier 				{$1 ++ [$2]}
			| modifier					{[$1]}

modifier :: {Modifier}			
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
					

functiondecl		: functionhead functionbody				{MemberFunction(fst $1, fst (snd $1), fst (snd (snd $1)), snd (snd (snd $1)), $2)}

functionhead		: functiontype identifier functionparameters	{($1,($2,($3,[])))}
			| modifiers functiontype identifier functionparameters	{($2,($3,($4,[])))}

functionparameters	: '(' formalparameters ')'			{$2}
			| '(' ')'					{[]}

formalparameters	: formalparameters ',' formalparameter		{$1 ++ [$3]}
			| formalparameter				{[$1]}
			
formalparameter		: type identifier				{($1, $2)}

functionbody 		: statement					{$1}

statement		: '{' statements '}'				{Block($2)}
			| '{' '}'					{Block([])}
			| if '(' exp ')' statement			{If($3, $5, Nothing)}
			| if '(' exp ')' statement else statement	{If($3, $5, Just $7)}
			| type simplename				{LocalVarDecl($1, $2)}
			| return exp 					{Return($2)}
			| while '(' exp ')' statement			{While($3, $5)}
			| do statement while '(' exp ')'		{Do($2, $5)}
			| statementexpstatement				{$1}
			

statements		: statements ';' statement			{$1 ++ [$3]}
			| statement					{[$1]}

statementexpstatement	: assignmentstatement				{StatementExpStatement($1)}
			| newstatement					{StatementExpStatement($1)}
			
newstatement		: new type '(' parameters ')'		{New($2, $4)}
			| new type '(' ')'			{New($2, [])}
			
assignmentstatement	: exp '=' exp 		{Assign($1,$3,"=")}
			| exp '+=' exp 		{Assign($1,$3,"+=")}
			| exp '-=' exp 		{Assign($1,$3,"-=")}
			| exp '*=' exp 		{Assign($1,$3,"*=")}
			| exp '/=' exp 		{Assign($1,$3,"/=")}
			| exp '%=' exp 		{Assign($1,$3,"%=")}
			| exp '<<=' exp 	{Assign($1,$3,"<<=")}
			| exp '>>=' exp 	{Assign($1,$3,">>=")}
			| exp '>>>=' exp 	{Assign($1,$3,">>>=")}
			| exp '&=' exp 		{Assign($1,$3,"&=")}
			| exp '|=' exp 		{Assign($1,$3,"|=")}
			| exp '^=' exp 		{Assign($1,$3,"^=")}

parameters		: parameters ',' exp 	{$1 ++ [$3]}
			| exp			{[$1]}
			
			
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
			
			
			
			
exp 			: infixexp		{$1}
			| intliteral		{$1}
			| '(' type ')' exp	{Cast($2, $4)}
			| '(' exp ')'		{$2}
			| stringliteral		{String($1)}
			| boolliteral		{Boolean($1)}
			| null			{Null}
			| unaryexp		{$1}
			
infixexp		: exp '+' exp 		{Infix("+", $1,$3)}
			| exp '*' exp 		{Infix("*", $1,$3)}
			| exp '-' exp 		{Infix("-", $1,$3)}
			| exp '/' exp 		{Infix("/", $1,$3)}
			| exp '%' exp		{Infix("%", $1,$3)}
			| exp '<<' exp		{Infix("<<", $1,$3)}
			| exp '>>' exp		{Infix(">>", $1,$3)}
			| exp '>>>' exp		{Infix(">>>", $1,$3)}
			| exp '&' exp		{Infix("&", $1,$3)}
			| exp '|' exp		{Infix("|", $1,$3)}
			| exp '^' exp		{Infix("^", $1,$3)}
			| exp '&&' exp		{Infix("&&", $1,$3)}
			| exp '||' exp		{Infix("||", $1,$3)}
			
unaryexp		: '-' exp %prec UNARY	{Unary("-", $2)}
			| '+' exp %prec UNARY	{Unary("+", $2)}
			| '!' exp %prec UNARY	{Unary("!", $2)}
			
			
			

intliteral		: literal_int		{Integer(read $1)}

{

data ClassElement a
	= Field a
	| Function a
	| Constructor a

data StatementOrExp a
	= Statement a
	| Exp a




parseError :: [Token] -> a
parseError t = error (show t)

main = do
    putStrLn (drawAst(parseJava(alexScanTokens "public static class test { void t (int g, int h) {if (1 - 2 * 2 - 2) {}} } ")))
}
