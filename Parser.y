{
module Parser (parseJava) where
import Scanner
import Abs
import Data.Maybe
}

%name parseJava
%tokentype {Token}
%error {parseError}


-- 2 shift/reduce Konflikt durch if-else Konstrukt, kein Problem da shift gewünschte Operation ist
-- 2 shift/reduce Konflikt durch verschiedene Casts
%token
      class			{ClassToken _}
      '{' 			{LeftBracesToken _}
      '}'			{RightBracesToken _}
      identifier		{IdentifierToken $$ _}
      public 			{PublicToken _}
      private			{PrivateToken _}
      final 			{FinalToken _}
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
      '++'			{PlusPlusToken _}
      '--'			{MinusMinusToken _}
      '!'			{NotToken _}
      '~'			{BitComplementToken _}
      ';'			{SemicolonToken _}
      literal_int		{IntLiteralToken $$ _}	
      stringliteral		{StringLiteralToken $$ _}
      boolliteral		{BooleanLiteralToken $$ _}
      if 			{IfToken _}
      else 			{ElseToken _}
      return 			{ReturnToken _}
      while 			{WhileToken _}
      do 			{DoToken _}
      for 			{ForToken _}
      new 			{NewToken _}
      this 			{ThisToken _}
      super 			{SuperToken _}
      break 			{BreakToken _}
      continue 			{ContinueToken _}
      
      
      
      
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
%right UNARY '!' '~' '++' '--' CAST NEW
%left '[' ']' '(' ')' '.'
      
%%
classdef 		: class identifier classbody			{ ClassDef($2, [], [], [], []) }
			| modifiers class identifier classbody		{ ClassDef($3, $1, fst $4, [], [])}

			
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
			| fielddecl  {Field $1}
			
fielddecl		: type identifier ';' 				{MemberField([],$1, $2, Nothing)}
			| modifiers type identifier ';' 		{MemberField($1, $2, $3, Nothing)}
			| type identifier '=' exp ';' 			{MemberField([],$1, $2, Just $4)}
			| modifiers type identifier '=' exp ';' 	{MemberField($1, $2, $3, Just $5)}
			
			
			
functiondecl		: functionhead statement				{MemberFunction(fst $1, fst (snd $1), fst (snd (snd $1)), snd (snd (snd $1)), $2)}

functionhead		: type identifier functionparameters	{($1,($2,($3,[])))}
			| modifiers type identifier functionparameters	{($2,($3,($4,$1)))}
			| void identifier functionparameters	{("void",($2,($3,[])))}
			| modifiers void identifier functionparameters	{("void",($3,($4,$1)))}
			

functionparameters	: '(' formalparameters ')'			{$2}
			| '(' ')'					{[]}

formalparameters	: formalparameters ',' formalparameter		{$1 ++ [$3]}
			| formalparameter				{[$1]}
			
formalparameter		: type identifier				{($1, $2)}
			
modifiers 		: modifiers modifier 				{$1 ++ [$2]}
			| modifier					{[$1]}

			
modifier		: public 					{Public}
			| private 					{Private}

statement		: type identifier ';'				{LocalVarDecl($1, $2, Nothing)}
			| return exp ';' 				{Return($2)}
			| if '(' exp ')' statement			{If($3, $5, Nothing)}
			| if '(' exp ')' statement else statement	{If($3, $5, Just $7)}
			| '{' statements '}'				{Block($2)}
			| '{' '}'					{Block([])}
			| type identifier '=' exp ';'			{LocalVarDecl($1, $2, Just $4)}
			| final type identifier '=' exp ';'		{LocalFinalDecl($2, $3, $5)}
			| while '(' exp ')' insideloopstatement 			{While($3, $5)}
			| do insideloopstatement while '(' exp ')'		{Do($2, $5)}
			| for '(' forinitstatement exp ';' forincstatement ')' insideloopstatement	{For($3, Just $4, Just $6, $8)}
			| for '(' forinitstatement ';' forincstatement ')' insideloopstatement		{For($3, Nothing, Just $5, $7)}
			| for '(' forinitstatement exp ';' ')' insideloopstatement			{For($3, Just $4, Nothing, $7)}
			| for '(' forinitstatement ';' ')' insideloopstatement				{For($3, Nothing, Nothing, $6)}
			| statementexpstatement				{$1}
			| ';'						{EmptyStatement}
			| methodeorinstance ';'				
			{
			case $1 of
					(StatementExpExp(MethodCall (e,n,p))) -> StatementExpStatement(MethodCall(e,n,p))
					_ -> error "not a statement"
			}
			| break ';'					{error "break outside of loop or conditional"}
			| continue ';'					{error "continue outside of loop"}
			
			
insideloopstatement	: type identifier ';'				{LocalVarDecl($1, $2, Nothing)}
			| return exp ';' 				{Return($2)}
			| if '(' exp ')' insideloopstatement			{If($3, $5, Nothing)}
			| if '(' exp ')' insideloopstatement else insideloopstatement	{If($3, $5, Just $7)}
			| '{' insideloopstatements '}'				{Block($2)}
			| '{' '}'					{Block([])}
			| type identifier '=' exp ';'			{LocalVarDecl($1, $2, Just $4)}
			| final type identifier '=' exp ';'		{LocalFinalDecl($2, $3, $5)}
			| while '(' exp ')' insideloopstatement 			{While($3, $5)}
			| do insideloopstatement while '(' exp ')'		{Do($2, $5)}
			| for '(' forinitstatement exp ';' forincstatement ')' insideloopstatement	{For($3, Just $4, Just $6, $8)}
			| for '(' forinitstatement ';' forincstatement ')' insideloopstatement		{For($3, Nothing, Just $5, $7)}
			| for '(' forinitstatement exp ';' ')' insideloopstatement			{For($3, Just $4, Nothing, $7)}
			| for '(' forinitstatement ';' ')' insideloopstatement				{For($3, Nothing, Nothing, $6)}
			| statementexpstatement				{$1}
			| ';'						{EmptyStatement}
			| methodeorinstance ';'				
			{
			case $1 of
					(StatementExpExp(MethodCall (e,n,p))) -> StatementExpStatement(MethodCall(e,n,p))
					_ -> error "not a statement"
			}
			| break ';'					{Break}
			| continue ';'					{Continue}
			
			
forinitstatement	: type identifier '=' exp ';'			{LocalVarDecl($1, $2, Just $4)}
			| ';'						{EmptyStatement}
			| lefthandside '=' exp ';'				{StatementExpStatement(Assign($1,$3, "="))}
			
forincstatement		: lefthandside '++'					{(PostfixUnary("++", $1))}
			| '++' lefthandside					{(PrefixUnary("++", $2))}
			| lefthandside '--'					{(PostfixUnary("--", $1))}
			| '--' lefthandside					{(PrefixUnary("--", $2))}
			| assignmentstatement				{($1)}
			| methodeorinstance
			{
			case $1 of
					(StatementExpExp(MethodCall (e,n,p))) -> (MethodCall(e,n,p))
					_ -> error "not a statement"
			}
			
statementexpstatement	: assignmentstatement ';'			{StatementExpStatement($1)}
			| newstatement ';'				{StatementExpStatement($1)}
			| lefthandside '++' ';'					{StatementExpStatement(PostfixUnary("++", $1))}
			| '++' lefthandside ';'					{StatementExpStatement(PrefixUnary("++", $2))}
			| lefthandside '--' ';'					{StatementExpStatement(PostfixUnary("--", $1))}
			| '--' lefthandside ';'					{StatementExpStatement(PrefixUnary("--", $2))}
			

			
			
			
			
newstatement		: new type '(' parameters ')' %prec NEW		{New($2, $4)}
			| new type '(' ')' %prec NEW			{New($2, [])}
			
parameters		: parameters ',' exp 	{$1 ++ [$3]}
			| exp			{[$1]}
			

assignmentstatement	: lefthandside '=' exp 		{Assign($1,$3,"=")}
			| lefthandside '+=' exp 		{Assign($1,$3,"+=")}
			| lefthandside '-=' exp 		{Assign($1,$3,"-=")}
			| lefthandside '*=' exp 		{Assign($1,$3,"*=")}
			| lefthandside '/=' exp 		{Assign($1,$3,"/=")}
			| lefthandside '%=' exp 		{Assign($1,$3,"%=")}
			| lefthandside '<<=' exp 	{Assign($1,$3,"<<=")}
			| lefthandside '>>=' exp 	{Assign($1,$3,">>=")}
			| lefthandside '>>>=' exp 	{Assign($1,$3,">>>=")}
			| lefthandside '&=' exp 		{Assign($1,$3,"&=")}
			| lefthandside '|=' exp 		{Assign($1,$3,"|=")}
			| lefthandside '^=' exp 		{Assign($1,$3,"^=")}
			

statements		: statements statement				{$1 ++ [$2]}
			| statement					{[$1]}
			
insideloopstatements		: insideloopstatements insideloopstatement		{$1 ++ [$2]}
				| insideloopstatement					{[$1]}
			
			
exp 			: infixexp		{$1}
			| intliteral		{$1}
			| lefthandside '++'		{StatementExpExp(PostfixUnary("++", $1))}
			| lefthandside '--'		{StatementExpExp(PostfixUnary("--", $1))}
			| '++' lefthandside		{StatementExpExp(PrefixUnary("++", $2))}
			| '--' lefthandside		{StatementExpExp(PrefixUnary("--", $2))}
			| unaryexp		{$1}
			| this			{This}
			| super			{Super}
			| newstatement 		{StatementExpExp($1)}
			| stringliteral		{String($1)}
			| boolliteral		{Boolean($1)}
			| null			{Null}
			| '(' qualifiedname ')' exp %prec CAST	{Cast(fst $2, $4)}
			| '(' identifier ')' exp %prec CAST	{Cast($2, $4)}
			| '(' primitivetype ')' exp %prec CAST	{Cast($2, $4)}
			| '(' exp ')'		{$2}
			| lefthandside		{$1}

lefthandside		: methodeorinstance	{$1}	
			| qualifiedname		{snd $1}
			| simplename		{LocalOrFieldVar($1)}
			| '(' lefthandside ')'	{$2}
			
			
			
			

methodeorinstance		: identifier '(' parameters ')' {StatementExpExp(MethodCall(This, $1, $3))}
				| methodeorinstance '.' identifier '(' parameters ')' {StatementExpExp(MethodCall($1, $3, $5))}
				| qualifiedname '(' parameters ')' {StatementExpExp(MethodCall((snd $1), "", $3))}
				| identifier '('  ')' {StatementExpExp(MethodCall(This, $1, []))}
				| methodeorinstance '.' identifier '('  ')' {StatementExpExp(MethodCall($1, $3, []))}
				| methodeorinstance '.' identifier	{InstanceVar($1, $3)}
				| qualifiedname '('  ')' {StatementExpExp(MethodCall((snd $1), "", []))}
				| this '.' identifier	{InstanceVar(This, $3)}
				| this '.' identifier '(' ')'	{StatementExpExp(MethodCall(This, $3, []))}
				| this '.' identifier '(' parameters ')'	{StatementExpExp(MethodCall(This, $3, $5))}
				
				
				
				
				
			
			
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
			| exp '<' exp		{Infix("<", $1,$3)}
			| exp '>' exp		{Infix(">", $1,$3)}
			| exp '>=' exp		{Infix(">=", $1,$3)}
			| exp '<=' exp		{Infix("<=", $1,$3)}
			| exp '==' exp		{Infix("==", $1,$3)}
			| exp '!=' exp		{Infix("!=", $1,$3)}
			| lefthandside '=' exp 		{StatementExpExp(Assign($1,$3,"="))}
			| lefthandside '+=' exp 		{StatementExpExp(Assign($1,$3,"+="))}
			| lefthandside '-=' exp 		{StatementExpExp(Assign($1,$3,"-="))}
			| lefthandside '*=' exp 		{StatementExpExp(Assign($1,$3,"*="))}
			| lefthandside '/=' exp 		{StatementExpExp(Assign($1,$3,"/="))}
			| lefthandside '%=' exp 		{StatementExpExp(Assign($1,$3,"%="))}
			| lefthandside '<<=' exp 	{StatementExpExp(Assign($1,$3,"<<="))}
			| lefthandside '>>=' exp 	{StatementExpExp(Assign($1,$3,">>="))}
			| lefthandside '>>>=' exp 	{StatementExpExp(Assign($1,$3,">>>="))}
			| lefthandside '&=' exp 		{StatementExpExp(Assign($1,$3,"&="))}
			| lefthandside '|=' exp 		{StatementExpExp(Assign($1,$3,"|="))}
			| lefthandside '^=' exp 		{StatementExpExp(Assign($1,$3,"^="))}
			

unaryexp		: '-' exp %prec UNARY	{Unary("-", $2)}
			| '+' exp %prec UNARY	{Unary("+", $2)}
			| '!' exp %prec UNARY	{Unary("!", $2)}
			| '~' exp %prec UNARY	{Unary("~", $2)}
			
			
			
			
intliteral		: literal_int		{Integer(read $1)}

simplename		: identifier		{$1}

qualifiedname		: identifier '.' identifier	{(($1 ++ "." ++ $3), (InstanceVar(LocalOrFieldVar($1), $3)))}
			| qualifiedname '.' identifier	{(((fst $1) ++ "." ++ $3), (InstanceVar((snd $1), $3)))}

type 			: primitivetype			{$1}
			| referencetype			{$1}
					
primitivetype		: boolean		{"boolean"}
			| integer		{"int"}
			| char 			{"char"}
					
referencetype		: qualifiedname		{fst $1}
			| simplename		{$1}




			



{

data ClassElement 
	= Field MemberField
	| Function MemberFunction

data StatementOrExp a
	= Statement a
	| Exp a




parseError :: [Token] -> a
parseError t = error (show t)

}
