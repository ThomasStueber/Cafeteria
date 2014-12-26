{
module Parser (parseJava) where
import Scanner
import Abs
import Data.Maybe
}

%name parseJava
%tokentype { Token }
%error { parseError }
  
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
      ':'			{ConditionalColonToken _}
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
      switch 			{SwitchToken _}
      case 			{CaseToken _}
      instanceof 		{InstanceOfToken _}
      static 			{StaticToken _}
      protected 		{ProtectedToken _}
      abstract	 		{AbstractToken _}
      '?'	 		{ConditionalQuestionmarkToken _}
      

%%

compilationunit  : typedeclarations { $1 }

typedeclarations : typedeclaration {[$1]}
		 | typedeclarations typedeclaration {$1 ++ [$2]}

name             : qualifiedname {$1}
		 | simplename {$1}

typedeclaration  : classdeclaration {$1}

qualifiedname    : name  '.' identifier {$1 ++ $3}

simplename       : identifier {$1}

classdeclaration : class identifier classbody {ClassDef $2 [] (snd (snd $3)) (fst (snd $3)) (fst $3)}
                 | modifiers class identifier classbody {ClassDef $3 $1 (snd (snd $4)) (fst (snd $4)) (fst $4)}

classbody        : '{' '}'  { ([], ([],[])) }
		 | '{' classbodydeclarations  '}' { $2 }

modifiers        : modifier {[$1]}
		 | modifiers modifier		 {$1 ++ [$2]}

classbodydeclarations :  classbodydeclaration {$1}
		 | classbodydeclarations classbodydeclaration{((fst $1) ++ (fst $2) , ((fst (snd $1)) ++ (fst (snd $2)), (snd (snd $1)) ++ (snd (snd $2))))}

modifier         : public {Public}
		 | protected {Protected}
                 | private {Private}
                 | static {Static}
                 | abstract {Abstract}
                 | final {Final}

classtype        : classorinterfacetype{$1}

classbodydeclaration : classmemberdeclaration {$1}
		 | constructordeclaration {([$1], ([],[]))}

classorinterfacetype : name{$1}

classmemberdeclaration : fielddeclaration {([], ($1, []))}
		 | methoddeclaration {([], ([], [$1]))}

constructordeclaration : constructordeclarator constructorbody {Constructor [] (fst $1) (Block $2) (snd $1)}
		 |  modifiers constructordeclarator constructorbody 
		 {Constructor $1 (fst $2) (Block $3) (snd $2)}

fielddeclaration : type variabledeclarators  ';' {fieldDeclHelper ($1, [], $2)}
 		 | modifiers type variabledeclarators  ';' {fieldDeclHelper ($2, $1, $3)}

methoddeclaration : methodheader methodbody {MemberFunction (fst (snd $1)) (fst (snd (snd $1))) (snd (snd (snd $1))) (fst $1) $2}

block            : '{'   '}' {Block []}
		 | '{'  blockstatements  '}' {Block $2}

constructordeclarator :  simplename '('  ')'  {($1, [])}
		 |  simplename '(' formalparameterlist ')'  {($1, $3)}

constructorbody	 : '{' '}' {[]}
		 | '{' explicitconstructorinvocation  '}' {[$2]}
		 | '{' blockstatements  '}' {$2}
		 | '{' explicitconstructorinvocation blockstatements '}' {[$2] ++ $3}

methodheader	 : type methoddeclarator {([], ($1, $2))}
		 | modifiers type methoddeclarator {($1, ($2, $3))}
		 | void methoddeclarator {([], ("void", $2))}
		 | modifiers void methoddeclarator {($1, ("void", $3))}

type             : primitivetype {$1}
		 | referencetype {$1}

variabledeclarators : variabledeclarator {[$1]}
		 | variabledeclarators  ','  variabledeclarator {$1 ++ [$3]}

methodbody       : block {$1}
		 | ';' {EmptyStatement}

blockstatements  : blockstatement {$1}
		 | blockstatements blockstatement {$1 ++ $2}

formalparameterlist : formalparameter {[$1]}
		 | formalparameterlist  ','  formalparameter{$1 ++ [$3]}

explicitconstructorinvocation : this '('  ')'   ';'  {ConstructorInvocation []}
		 | this '(' argumentlist  ')'   ';'  {ConstructorInvocation $3}

classtypelist    : classtype {}
		 | classtypelist  ','  classtype { }

methoddeclarator : identifier '('  ')'  {($1, [])}
		 | identifier '(' formalparameterlist  ')'  {($1, $3)}

primitivetype    : boolean {"boolean"}
		 | numerictype {$1}

referencetype    : classorinterfacetype {$1}


variabledeclarator : variabledeclaratorid {($1, Nothing)}
		 | variabledeclaratorid '=' variableinitializer {($1, Just $3)}
		 
blockstatement	 : localvariabledeclarationstatement {$1}
		 | statement  {[$1]}

formalparameter  : type variabledeclaratorid {($1, $2)}

argumentlist     : expression {[$1]}
		 | argumentlist  ','  expression {$1 ++ [$3]}

numerictype      : integraltype {$1}

variabledeclaratorid : identifier {$1}

variableinitializer  : expression {$1}

localvariabledeclarationstatement : localvariabledeclaration  ';'  {$1}

statement        : statementwithouttrailingsubstatement{$1}
		 | ifthenstatement {$1}
		 | ifthenelsestatement {$1}
		 | whilestatement {$1}
				     

expression       : assignmentexpression {$1}

integraltype     : integer  {"int"}
                 | char {"char"}

localvariabledeclaration : type variabledeclarators {varDeclHelper ($1, $2)}

statementwithouttrailingsubstatement : block {$1}
		 | emptystatement {$1}
		 | expressionstatement {StatementExpStatement $1}
		 | returnstatement {$1}

ifthenstatement  : if '(' expression  ')'  statement {If $3 $5 Nothing}

ifthenelsestatement : if '(' expression  ')' statementnoshortif else statement  {If $3 $5 (Just $7)}

whilestatement   : while '(' expression  ')'  statement {While $3 $5}

assignmentexpression : conditionalexpression {$1}
		 |  assignment{StatementExpExp $1}

emptystatement	 :  ';'  {EmptyStatement}

expressionstatement : statementexpression  ';' {$1}

returnstatement  : return  ';'  {Return Nothing}
		 | return expression  ';' {Return (Just $2)}

statementnoshortif : statementwithouttrailingsubstatement {$1}
		 | ifthenelsestatementnoshortif {$1}
		 | whilestatementnoshortif {$1}

conditionalexpression : conditionalorexpression {$1}
		 | conditionalorexpression '?' expression  ':'  conditionalexpression {ConditionalExp $1 $3 $5}

assignment       :lefthandside assignmentoperator assignmentexpression {Assign $1 $3 $2}
	

statementexpression : assignment {$1}
		 | preincrementexpression {$1}
		 | predecrementexpression {$1}
		 | postincrementexpression {$1}
		 | postdecrementexpression {$1}
		 | methodinvocation {$1}
		 | classinstancecreationexpression {$1}

ifthenelsestatementnoshortif :if '(' expression  ')'  statementnoshortif
			      else statementnoshortif  {If $3 $5 (Just $7)}

whilestatementnoshortif : while '(' expression  ')'  statementnoshortif {While $3 $5}

conditionalorexpression : conditionalandexpression {$1}
		 | conditionalorexpression '||' conditionalandexpression{Infix "||" $1 $3}

lefthandside     	: name {LocalOrFieldVar $1} --noch falsch
			| fieldaccess {$1}

assignmentoperator : '=' {"="}
		 | '*=' {"*="}
		 | '/=' {"/="}
		 | '%=' {"%="}
		 | '+=' {"+="}
		 | '-=' {"-="}
		 | '<<=' {"<<="}
		 | '>>=' {">>="}
		 | '>>>=' {">>>="}
		 | '&=' {"&="}
		 | '^=' {"^="}
		 | '|='{"|="}

preincrementexpression : '++' unaryexpression {PostfixUnary "++" $2}

predecrementexpression : '--' unaryexpression {PrefixUnary "--" $2}

postincrementexpression : postfixexpression '++' {PostfixUnary "++" $1}

postdecrementexpression : postfixexpression '--' {PostfixUnary "--" $1}

methodinvocation : name '('   ')'  {MethodCall This $1 []}
		 | name '(' argumentlist ')' {MethodCall This $1 $3}
		 | primary  '.' identifier '(' ')'  {MethodCall $1 $3 []}
		 | primary  '.' identifier '(' argumentlist  ')'  {MethodCall $1 $3 $5}
     
classinstancecreationexpression : new classtype '('   ')'  {New $2 []}
                 | new classtype '('  argumentlist  ')'  {New $2 $4}

conditionalandexpression : inclusiveorexpression {$1}
		      | conditionalandexpression '&&' inclusiveorexpression{Infix "&&" $1 $3}

fieldaccess      : primary  '.' identifier {InstanceVar $1 $3}

unaryexpression	 : preincrementexpression {StatementExpExp $1}
		 | predecrementexpression {StatementExpExp $1}
		 | '+' unaryexpression {Unary "+" $2}
		 | '-' unaryexpression {Unary "-" $2}
		 | unaryexpressionnotplusminus {$1}

postfixexpression : primary {$1}
		 | name {LocalOrFieldVar $1} --noch falsch
		 | postincrementexpression {StatementExpExp $1}
		 | postdecrementexpression{StatementExpExp $1}

primary		 : primarynonewarray {$1}

inclusiveorexpression : exclusiveorexpression {$1}
		 | inclusiveorexpression '|' exclusiveorexpression {Infix "|" $1 $3}

primarynonewarray : literal {$1}
		 | this {This}
		 | '(' expression ')'  {$2}
                 | classinstancecreationexpression {StatementExpExp $1}
		 | fieldaccess {$1}
		 | methodinvocation {StatementExpExp $1}

unaryexpressionnotplusminus : postfixexpression {$1}
	         | '~' unaryexpression {Unary "~" $2}
		 | '!' unaryexpression {Unary "!" $2}
		 | castexpression{$1}

exclusiveorexpression : andexpression {$1}
		 | exclusiveorexpression '^' andexpression {Infix "^" $1 $3}

literal		 : boolliteral {Boolean $1}
		 | literal_int {Integer (read $1)}
		 -- | CHARLITERAL { }
		 | stringliteral {String $1}
		 | null {Null}

castexpression	 : '('  primitivetype  ')'  unaryexpression {Cast $2 $4} --noch falsch
 		 | '('  expression  ')'  unaryexpressionnotplusminus{LocalOrFieldVar ""} -- noch falsch

andexpression    : equalityexpression {$1}
		 | andexpression '&' equalityexpression {Infix "&" $1 $3}

equalityexpression : relationalexpression {$1}
		 | equalityexpression '==' relationalexpression {Infix "==" $1 $3}
		 | equalityexpression '!=' relationalexpression {Infix "!=" $1 $3}

relationalexpression : shiftexpression {$1}
		 | relationalexpression '<' shiftexpression {Infix "<" $1 $3}
		 | relationalexpression '>' shiftexpression {Infix ">" $1 $3}
		 | relationalexpression '<=' shiftexpression {Infix "<=" $1 $3}
		 | relationalexpression '>=' shiftexpression {Infix ">=" $1 $3}
		 -- | relationalexpression instanceof referencetype {Infix "instanceof" $1 $3}

shiftexpression	 	: additiveexpression {$1}
			| shiftexpression '<<' additiveexpression {Infix "<<" $1 $3}
			| shiftexpression '>>' additiveexpression {Infix ">>" $1 $3}
			| shiftexpression '>>>' additiveexpression {Infix ">>>" $1 $3}
			

additiveexpression : multiplicativeexpression {$1}
		 | additiveexpression '+' multiplicativeexpression {Infix "+" $1 $3}
		 | additiveexpression '-' multiplicativeexpression {Infix "-" $1 $3}

multiplicativeexpression : unaryexpression {$1}
		 | multiplicativeexpression '*' unaryexpression {Infix "*" $1 $3}
		 | multiplicativeexpression '/' unaryexpression {Infix "/" $1 $3}
		 | multiplicativeexpression '%' unaryexpression {Infix "%" $1 $3}


{

-- Hilfsfunktion für Statements wie "int i, i2, i3"
varDeclHelper :: (String, [(String, Maybe Exp)]) -> [Statement]
varDeclHelper (t, []) = []
varDeclHelper (t, x:xs) = [LocalVarDecl t (fst x) (snd x)] ++ varDeclHelper (t, xs)

-- Hilfsfunktion für Felddeklerationen wie "int i, i2, i3"
fieldDeclHelper :: (String, [Modifier], [(String, Maybe Exp)]) -> [MemberField]
fieldDeclHelper (t, m, []) = []
fieldDeclHelper (t, m, x:xs) = [MemberField m t (fst x) (snd x)] ++ fieldDeclHelper (t, m, xs)

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
