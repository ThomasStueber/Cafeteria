{
module Parser (parseJava) where
import Scanner
import Abs
import Data.Maybe
import Data.String.Utils
import Data.Bits
import Data.Word
import Data.List
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
      intliteral		{IntLiteralToken $$ _}	
      stringliteral		{StringLiteralToken $$ _}
      boolliteral		{BooleanLiteralToken $$ _}
      charliteral		{CharLiteralToken $$ _}
      if 			{IfToken _}
      else 			{ElseToken _}
      return 			{ReturnToken _}
      while 			{WhileToken _}
      do 			{DoToken _}
      for 			{ForToken _}
      new 			{NewToken _}
      this 			{ThisToken _}
      break 			{BreakToken _}
      continue 			{ContinueToken _}
      switch 			{SwitchToken _}
      case 			{CaseToken _}
      default			{DefaultToken _}
      instanceof 		{InstanceOfToken _}
      static 			{StaticToken _}
      protected 		{ProtectedToken _}
      abstract	 		{AbstractToken _}
      assert 			{AssertToken _}
      '?'	 		{ConditionalQuestionmarkToken _}
      

%%
compilationunit :: {[ClassDef]}
compilationunit  : typedeclarations { $1 }

typedeclarations :: {[ClassDef]}
typedeclarations : typedeclaration {[$1]}
		 | typedeclarations typedeclaration {$1 ++ [$2]}

name :: {String}
name             : qualifiedname {$1}
		 | simplename {$1}

typedeclaration :: {ClassDef}
typedeclaration  : classdeclaration {$1}

qualifiedname :: {String}
qualifiedname  : name  '.' identifier {$1 ++ "." ++ $3}

simplename :: {String}
simplename  : identifier {$1}

classdeclaration :: {ClassDef}
classdeclaration : class identifier classbody {ClassDef $2 [] (fst (snd $3)) (snd (fst $3)) (fst (fst $3)) (snd (snd $3))}
                 | modifiers class identifier classbody {ClassDef $3 (checkModifiers $1) (fst (snd $4)) (snd (fst $4)) (fst (fst $4)) (snd (snd $4))}

classbody :: {(([Constructor],[MemberField]),([MemberFunction],[Statement]))}
classbody        : '{' '}'  { (([], []), ([],[])) }
		 | '{' classbodydeclarations  '}' { $2 }

modifiers :: {[Modifier]}
modifiers        : modifier {[$1]}
		 | modifiers modifier		 {$1 ++ [$2]}


classbodydeclarations :: {(([Constructor],[MemberField]),([MemberFunction],[Statement]))}
classbodydeclarations :  classbodydeclaration {$1}
		 | classbodydeclarations classbodydeclaration{(((fst (fst $1)) ++ (fst (fst $2)), (snd (fst $1)) ++ (snd (fst $2))) , ((fst (snd $1)) ++ (fst (snd $2)), (snd (snd $1)) ++ (snd (snd $2))))}

modifier :: {Modifier}
modifier         : public {Public}
		 | protected {Protected}
                 | private {Private}
                 | static {Static}
                 | abstract {Abstract}
                 | final {Final}

classtype :: {String}
classtype        : classorinterfacetype{$1}

classbodydeclaration :: {(([Constructor],[MemberField]),([MemberFunction],[Statement]))}
classbodydeclaration : classmemberdeclaration 	{$1}
		     | constructordeclaration 	{(([$1], []), ([],[]))}
		     | staticblock		{(([],[]), ([], [$1]))}
		 
		 
staticblock :: {Statement}
staticblock : static block			{StaticBlock $2}
	    | block				{$1}

classorinterfacetype :: {String}
classorinterfacetype : name{$1}

classmemberdeclaration :: {(([Constructor],[MemberField]),([MemberFunction],[Statement]))}
classmemberdeclaration : fielddeclaration {(([], $1), ([], []))}
		 | methoddeclaration {(([], []), ([$1], []))}

constructordeclaration :: {Constructor}
constructordeclaration : constructordeclarator constructorbody {Constructor [] (fst $1) (Block $2) (snd $1)}
		 |  modifiers constructordeclarator constructorbody 
		 {Constructor (checkModifiers $ checkModifiersConstructor $1) (fst $2) (if (unreachableCode $3 False) then error "In einem Block kommt nicht erreichbarer Code vor" else Block $3) (snd $2)}

fielddeclaration :: {[MemberField]}
fielddeclaration : type variabledeclarators  ';' {fieldDeclHelper $1 [] $2}
 		 | modifiers type variabledeclarators  ';' {fieldDeclHelper $2 (checkModifiers $ checkModifiersField $1) $3}

methoddeclaration :: {MemberFunction}
methoddeclaration : methodheader methodbody {MemberFunction (fst (snd $1)) (fst (snd (snd $1))) (snd (snd (snd $1))) (fst $1) $2}

block :: {Statement}
block            : '{'   '}' {Block []}
		 | '{'  blockstatements  '}' {if (unreachableCode $2 False) then error "In einem Block kommt nicht erreichbarer Code vor" else Block $2}
		 
block_innerloop :: {Statement}
block_innerloop  : '{'   '}' {Block []}
		 | '{'  blockstatements_innerloop  '}' {if (unreachableCode $2 False) then error "In einem Block kommt nicht erreichbarer Code vor" else Block $2}

constructordeclarator :: {(String, [(String, String)])}
constructordeclarator :  simplename '('  ')'  {($1, [])}
		 |  simplename '(' formalparameterlist ')'  {($1, $3)}

constructorbody :: {[Statement]}
constructorbody	 : '{' '}' {[]}
		 | '{' explicitconstructorinvocation  '}' {[$2]}
		 | '{' blockstatements  '}' {$2}
		 | '{' explicitconstructorinvocation blockstatements '}' {[$2] ++ $3}

methodheader :: {([Modifier], (String, (String, [(String, String)])))}
methodheader	 : type methoddeclarator {([], ($1, $2))}
		 | modifiers type methoddeclarator {((checkModifiers $1), ($2, $3))}
		 | void methoddeclarator {([], ("void", $2))}
		 | modifiers void methoddeclarator {((checkModifiers $1), ("void", $3))}

type :: {String}
type             : primitivetype {$1}
		 | referencetype {$1}

variabledeclarators :: {[(String, Maybe Exp)]}
variabledeclarators : variabledeclarator {[$1]}
		 | variabledeclarators  ','  variabledeclarator {$1 ++ [$3]}

methodbody :: {Statement}
methodbody       : block {$1}
		 | ';' {EmptyStatement}

blockstatements :: {[Statement]}
blockstatements  : blockstatement {$1}
		 | blockstatements blockstatement {$1 ++ $2}
		
blockstatements_innerloop :: {[Statement]} 
blockstatements_innerloop  : blockstatement_innerloop {$1}
		 | blockstatements_innerloop blockstatement_innerloop {$1 ++ $2}

formalparameterlist :: {[(String, String)]}
formalparameterlist : formalparameter {[$1]}
		 | formalparameterlist  ','  formalparameter{$1 ++ [$3]}

explicitconstructorinvocation :: {Statement}
explicitconstructorinvocation : this '('  ')'   ';'  		  {ConstructorInvocation []}
			      | this '(' argumentlist  ')'   ';'  {ConstructorInvocation $3}


methoddeclarator :: {(String, [(String, String)])}
methoddeclarator : identifier '('  ')'  {($1, [])}
		 | identifier '(' formalparameterlist  ')'  {($1, $3)}

primitivetype :: {String}
primitivetype  : boolean {"boolean"}
	       | numerictype {$1}

referencetype :: {String}
referencetype  : classorinterfacetype {$1}

variabledeclarator :: {(String, Maybe Exp)}
variabledeclarator : variabledeclaratorid {($1, Nothing)}
		 | variabledeclaratorid '=' variableinitializer {($1, Just $3)}
		 
blockstatement :: {[Statement]}
blockstatement	: localvariabledeclarationstatement {$1}
		| statement  {[$1]}
		 
blockstatement_innerloop :: {[Statement]}
blockstatement_innerloop  : localvariabledeclarationstatement {$1}
			  | statement_innerloop  {[$1]}

formalparameter :: {(String, String)}
formalparameter  : type variabledeclaratorid {($1, $2)}

argumentlist :: {[Exp]}
argumentlist     : expression {[$1]}
		 | argumentlist  ','  expression {$1 ++ [$3]}

numerictype :: {String}
numerictype      : integraltype {$1}

variabledeclaratorid :: {String}
variabledeclaratorid : identifier {$1}

variableinitializer :: {Exp}
variableinitializer  : expression {$1}

localvariabledeclarationstatement :: {[Statement]}
localvariabledeclarationstatement : localvariabledeclaration  ';'  {$1}

statement :: {Statement}
statement        : statementwithouttrailingsubstatement{$1}
		 | ifthenstatement {$1}
		 | ifthenelsestatement {$1}
		 | whilestatement {$1}
		 | forstatement {$1}
		 | identifier ':' statement {LabeledStatement $1 $3}

		 
statement_innerloop :: {Statement} 
statement_innerloop  : statementwithouttrailingsubstatement_innerloop{$1}
		     | ifthenstatement_innerloop {$1}
		     | ifthenelsestatement_innerloop {$1}
		     | whilestatement {$1}
		     | forstatement {$1}
		     | identifier ':' statement_innerloop {LabeledStatement $1 $3}
		
expression :: {Exp}
expression       : assignmentexpression {$1}

integraltype :: {String}
integraltype     : integer  {"int"}
                 | char {"char"}

localvariabledeclaration :: {[Statement]}
localvariabledeclaration : type variabledeclarators {varDeclHelper $1 False $2}
			 | final type variabledeclarators {varDeclHelper $2 True $3}

statementwithouttrailingsubstatement :: {Statement}
statementwithouttrailingsubstatement : block {$1}
		 | emptystatement {$1}
		 | expressionstatement {StatementExpStatement $1}
		 | returnstatement {$1}
		 | dowhilestatement {$1}
		 | assertstatement {$1}
		 | switchstatement {$1}

		 
statementwithouttrailingsubstatement_innerloop :: {Statement}
statementwithouttrailingsubstatement_innerloop : breakstatement		{$1}
		 | continue ';' {Continue Nothing}
		 | continue identifier ';' {Continue (Just $2)}
		 | block_innerloop {$1}
		 | emptystatement {$1}
		 | expressionstatement {StatementExpStatement $1}
		 | returnstatement {$1}
		 | dowhilestatement {$1}
		 | assertstatement {$1}
		 | switchstatement {$1}

ifthenstatement :: {Statement}
ifthenstatement  : if '(' expression  ')' statement {If $3 $5 Nothing}

ifthenelsestatement :: {Statement}
ifthenelsestatement : if '(' expression  ')' statementnoshortif else statement  {If $3 $5 (Just $7)}

ifthenstatement_innerloop :: {Statement}
ifthenstatement_innerloop  : if '(' expression  ')'  statement_innerloop {If $3 $5 Nothing}

ifthenelsestatement_innerloop :: {Statement}
ifthenelsestatement_innerloop : if '(' expression  ')' statementnoshortif_innerloop else statement_innerloop  {If $3 $5 (Just $7)}

whilestatement :: {Statement}
whilestatement   : while '(' expression  ')'  statement_innerloop {While $3 $5}

dowhilestatement :: {Statement}
dowhilestatement   : do statement_innerloop while '(' expression  ')' {Do $2 $5}

forstatement :: {Statement}
forstatement 	: for forcontrol statement_innerloop	{For (fst $2) (fst (snd $2)) (snd (snd $2)) $3}

forcontrol :: {([Statement], (Maybe Exp, [StatementExp]))}
forcontrol	: '(' forinit forbreakcondition ')'	{($2, ($3, []))}
		| '(' forinit forbreakcondition forincrement ')' {($2, ($3, $4))}
		
forinit :: {[Statement]}
forinit		: localvariabledeclarationstatement	{$1}
		| statementexpressions ';'		{map (\s -> StatementExpStatement s) $1}
		| ';'					{[]}
		
forbreakcondition :: {Maybe Exp}
forbreakcondition : ';'			{Nothing}
		  | expression ';'	{Just $1}
		  
forincrement :: {[StatementExp]}
forincrement : statementexpressions	{$1}


assignmentexpression :: {Exp}
assignmentexpression : conditionalexpression {$1}
		 |  assignment{StatementExpExp $1}

emptystatement :: {Statement}
emptystatement	 :  ';'  {EmptyStatement}

switchstatement :: {Statement}
switchstatement : switch '(' expression ')' switchblock	{Switch $3 $5}

switchblock :: {[Statement]}
switchblock : '{' '}'		{[]}
	    | '{' switchcaseblocks '}' {$2}
	    
switchcaseblocks :: {[Statement]}
switchcaseblocks : switchcaseblock {[$1]}
		 | switchcaseblocks switchcaseblock {$1 ++ [$2]}
	    
switchcaseblock :: {Statement}
switchcaseblock : switchblocklabels switchblockstatements {SwitchBlockStatement $1 $2}
	    
switchblockstatements :: {[Statement]}
switchblockstatements : switchblockstatement {[$1]}
		      | switchblockstatements switchblockstatement {$1 ++ [$2]}

switchblockstatement :: {Statement}
switchblockstatement : statement {$1}
		     | breakstatement {$1}

switchblocklabel :: {SwitchBlockLabel}
switchblocklabel : case expression ':' 	{CaseLabel $2}
		 | default ':'		{DefaultLabel}
		 
switchblocklabels :: {[SwitchBlockLabel]}
switchblocklabels : switchblocklabels switchblocklabel {$1 ++ [$2]}
		  | switchblocklabel {[$1]}
		     
breakstatement :: {Statement}
breakstatement 	: break ';'		{Break Nothing}
		| break identifier ';'	{Break (Just $2)}
		      
		      
assertstatement :: {Statement}
assertstatement		: assert expression ';'			{Assert $2 Nothing}
			| assert expression ':' expression ';' 	{Assert $2 (Just $4)}


expressionstatement :: {StatementExp}
expressionstatement : statementexpression  ';' {$1}

returnstatement :: {Statement}
returnstatement  : return  ';'  {Return Nothing}
		 | return expression  ';' {Return (Just $2)}

statementnoshortif :: {Statement}
statementnoshortif : statementwithouttrailingsubstatement {$1}
		 | ifthenelsestatementnoshortif {$1}
		 | whilestatementnoshortif {$1}
		 | forstatementnoshortif {$1}
		 | identifier ':' statementnoshortif {LabeledStatement $1 $3}
		 

statementnoshortif_innerloop :: {Statement}
statementnoshortif_innerloop : statementwithouttrailingsubstatement_innerloop {$1}
		 | ifthenelsestatementnoshortif_innerloop {$1}
		 | whilestatementnoshortif {$1}
		 | forstatementnoshortif {$1}
		 | identifier ':' statementnoshortif_innerloop {LabeledStatement $1 $3}
		 
conditionalexpression :: {Exp}
conditionalexpression : conditionalorexpression {$1}
		 | conditionalorexpression '?' expression  ':'  conditionalexpression 
		 {
		 case $1 of 
			(Boolean True) -> case $3 of 
					      (Integer r) -> (Integer r)
					      (Boolean r) -> (Boolean r)
					      (String r) -> (String r)
					      _ -> ConditionalExp $1 $3 $5
			(Boolean False) -> case $5 of 
					      (Integer r) -> (Integer r)
					      (Boolean r) -> (Boolean r)
					      (String r) -> (String r)
					      _ -> ConditionalExp $1 $3 $5
			_ -> ConditionalExp $1 $3 $5
		}

assignment :: {StatementExp}
assignment       :lefthandside assignmentoperator assignmentexpression {Assign $1 $3 $2}
	

statementexpression :: {StatementExp}
statementexpression : assignment {$1}
		 | preincrementexpression {$1}
		 | predecrementexpression {$1}
		 | postincrementexpression {$1}
		 | postdecrementexpression {$1}
		 | methodinvocation {$1}
		 | classinstancecreationexpression {$1}
		 
statementexpressions :: {[StatementExp]}
statementexpressions : statementexpression {[$1]}
		     | statementexpressions ',' statementexpression {$1 ++ [$3]}

ifthenelsestatementnoshortif :: {Statement}
ifthenelsestatementnoshortif :if '(' expression  ')'  statementnoshortif
			      else statementnoshortif  {If $3 $5 (Just $7)}
			      
ifthenelsestatementnoshortif_innerloop :: {Statement}
ifthenelsestatementnoshortif_innerloop :if '(' expression  ')'  statementnoshortif_innerloop
			      else statementnoshortif_innerloop  {If $3 $5 (Just $7)}

whilestatementnoshortif :: {Statement}
whilestatementnoshortif : while '(' expression  ')'  statementnoshortif_innerloop {While $3 $5}

forstatementnoshortif :: {Statement}
forstatementnoshortif 	: for forcontrol statementnoshortif_innerloop	{For (fst $2) (fst (snd $2)) (snd (snd $2)) $3}

conditionalorexpression :: {Exp}
conditionalorexpression : conditionalandexpression {$1}
		 | conditionalorexpression '||' conditionalandexpression
		 {
		 case $1 of 
			(Boolean l) -> case $3 of 
					      (Boolean r) -> (Boolean (l || r))
					      _ -> Infix "||" $1 $3
			_ -> Infix "||" $1 $3
		}

lefthandside :: {Exp}
lefthandside     	: fieldaccess {$1}
			| name {nameToInstanceVar $1}
			
assignmentoperator :: {String}
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

preincrementexpression :: {StatementExp}
preincrementexpression : '++' unaryexpression {PostfixUnary "++" $2}

predecrementexpression :: {StatementExp}
predecrementexpression : '--' unaryexpression {PrefixUnary "--" $2}

postincrementexpression :: {StatementExp}
postincrementexpression : postfixexpression '++' {PostfixUnary "++" $1}

postdecrementexpression :: {StatementExp}
postdecrementexpression : postfixexpression '--' {PostfixUnary "--" $1}

methodinvocation :: {StatementExp}
methodinvocation : name '('   ')'  {MethodCall This $1 []}
		 | name '(' argumentlist ')' {MethodCall This $1 $3}
		 | primary  '.' identifier '(' ')'  {MethodCall $1 $3 []}
		 | primary  '.' identifier '(' argumentlist  ')'  {MethodCall $1 $3 $5}
     
classinstancecreationexpression :: {StatementExp}
classinstancecreationexpression : new classtype '('   ')'  {New $2 []}
                 | new classtype '('  argumentlist  ')'  {New $2 $4}

conditionalandexpression :: {Exp}
conditionalandexpression : inclusiveorexpression {$1}
		      | conditionalandexpression '&&' inclusiveorexpression 
		      {
		      case $1 of 
			    (Boolean l) -> case $3 of 
						  (Boolean r) -> (Boolean (l && r))
						  _ -> Infix "&&" $1 $3
			    _ -> Infix "&&" $1 $3
		      }

fieldaccess :: {Exp}
fieldaccess      : primary  '.' identifier {InstanceVar $1 $3}


unaryexpression :: {Exp}
unaryexpression	 : preincrementexpression {StatementExpExp $1}
		 | predecrementexpression {StatementExpExp $1}
		 | '+' unaryexpression 
		 {
		  case $2 of 
			(Integer r) -> (Integer r)
			_ -> Unary "+" $2
		 }
		 | '-' unaryexpression 
		 {
		  case $2 of 
			(Integer r) -> (Integer (-r))
			_ -> Unary "-" $2
		 }
		 | unaryexpressionnotplusminus {$1}

postfixexpression :: {Exp}
postfixexpression : primary {$1}
		 | name {nameToInstanceVar $1} --noch falsch
		 | postincrementexpression {StatementExpExp $1}
		 | postdecrementexpression{StatementExpExp $1}

primary :: {Exp}
primary		 : primarynonewarray {$1}

inclusiveorexpression :: {Exp}
inclusiveorexpression : exclusiveorexpression {$1}
		 | inclusiveorexpression '|' exclusiveorexpression 
		 {
		  case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer ((.|.) l r))
					      _ -> Infix "|" $1 $3
			_ -> Infix "|" $1 $3
		 }

primarynonewarray :: {Exp}
primarynonewarray : literal {$1}
		 | this {This}
		 | '(' expression ')' {$2}
                 | classinstancecreationexpression {StatementExpExp $1}
		 | fieldaccess {$1}
		 | methodinvocation {StatementExpExp $1}

unaryexpressionnotplusminus :: {Exp}
unaryexpressionnotplusminus : postfixexpression {$1}
	         | '~' unaryexpression 
		 {
		  case $2 of 
			(Integer r) -> (Integer (complement r))
			_ -> Unary "~" $2
		 }
		 | '!' unaryexpression 
		 {
		  case $2 of 
			(Boolean r) -> (Boolean (not r))
			_ -> Unary "!" $2
		 }
		 | castexpression{$1}

exclusiveorexpression :: {Exp}
exclusiveorexpression : andexpression {$1}
		 | exclusiveorexpression '^' andexpression 
		 {
		  case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (xor l r))
					      _ -> Infix "^" $1 $3
			_ -> Infix "^" $1 $3
		 }

literal :: {Exp}
literal		 : boolliteral {Boolean $1}
		 | intliteral {Integer $1}
		 | charliteral {Char $1}
		 | stringliteral {String $1}
		 | null {Null}

castexpression :: {Exp}
castexpression	 : '('  primitivetype  ')'  unaryexpression 
		 {
		  case $2 of 
			"int" -> case $4 of 
					      (Integer r) -> (Integer r)
					      _ -> Cast $2 $4
			_ -> Cast $2 $4
		 }
 		 | '('  expression  ')'  unaryexpressionnotplusminus{Cast (expToName $2) $4} -- noch falsch

andexpression :: {Exp}
andexpression    : equalityexpression {$1}
		 | andexpression '&' equalityexpression 
		 {
		  case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer ((.&.) l r))
					      _ -> Infix "&" $1 $3
			_ -> Infix "&" $1 $3
		 }

equalityexpression :: {Exp}
equalityexpression : relationalexpression {$1}
		 | equalityexpression '==' relationalexpression {
		case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l == r))
					      _ -> Infix "==" $1 $3
			(Boolean l) -> case $3 of 
					      (Boolean r) -> (Boolean (l == r))
					      _ -> Infix "==" $1 $3
			_ -> Infix "==" $1 $3
		}
		 | equalityexpression '!=' relationalexpression 
		{
		case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l /= r)) 
					      _ -> Infix "!=" $1 $3
			(Boolean l) -> case $3 of 
					      (Boolean r) -> (Boolean (l /= r))
					      _ -> Infix "!=" $1 $3
			_ -> Infix "!=" $1 $3
		}

relationalexpression :: {Exp}
relationalexpression : shiftexpression {$1}
		 | relationalexpression '<' shiftexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l < r))
					      _ -> Infix "<" $1 $3
			_ -> Infix "<" $1 $3
		 }
		 | relationalexpression '>' shiftexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l > r))
					      _ -> Infix ">" $1 $3
			_ -> Infix ">" $1 $3
		 }
		 | relationalexpression '<=' shiftexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l <= r))
					      _ -> Infix "<=" $1 $3
			_ -> Infix "<=" $1 $3
		 }
		 | relationalexpression '>=' shiftexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Boolean (l >= r))
					      _ -> Infix ">=" $1 $3
			_ -> Infix ">=" $1 $3
		 }
		 | relationalexpression instanceof referencetype {InstanceOf $1 $3}

shiftexpression :: {Exp}
shiftexpression	 	: additiveexpression {$1}
			| shiftexpression '<<' additiveexpression 
			{
			case $1 of 
				(Integer l) -> case $3 of 
						      (Integer r) -> (Integer (shiftL l (mod r 32)))
						      _ -> Infix "<<" $1 $3
				_ -> Infix "<<" $1 $3
			}
			| shiftexpression '>>' additiveexpression 
			{
			case $1 of 
				(Integer l) -> case $3 of 
						      (Integer r) -> (Integer (shiftR l (mod r 32)))
						      _ -> Infix ">>" $1 $3
				_ -> Infix ">>" $1 $3
			}
			| shiftexpression '>>>' additiveexpression 
			{
			case $1 of 
				(Integer l) -> case $3 of 
						      (Integer r) -> if (r == 0) then (Integer r) else (Integer ((shiftR (fromIntegral l) (mod r 32)))) 
						      _ -> Infix ">>>" $1 $3
				_ -> Infix ">>>" $1 $3
			}
			

additiveexpression :: {Exp}
additiveexpression : multiplicativeexpression {$1}
		 | additiveexpression '+' multiplicativeexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (l+r))
					      (String r) -> (String ((show l) ++ r))
					      _ -> Infix "+" $1 $3
			(String l) -> case $3 of
					      (String r) -> (String (l++r))
					      (Integer r) -> (String (l ++ (show r)))
					      _ -> Infix "+" $1 $3
			_ -> Infix "+" $1 $3
		 }
		 | additiveexpression '-' multiplicativeexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (l-r))
					      _ -> Infix "-" $1 $3
			_ -> Infix "-" $1 $3
		 }

multiplicativeexpression :: {Exp}
multiplicativeexpression : unaryexpression {$1}
		 | multiplicativeexpression '*' unaryexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (l*r))
					      _ -> Infix "*" $1 $3
			_ -> Infix "*" $1 $3
		 }
		 | multiplicativeexpression '/' unaryexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (div l r))
					      _ -> Infix "/" $1 $3
			_ -> Infix "/" $1 $3
		 }
		 | multiplicativeexpression '%' unaryexpression 
		 {
		 case $1 of 
			(Integer l) -> case $3 of 
					      (Integer r) -> (Integer (mod l r))
					      _ -> Infix "%" $1 $3
			_ -> Infix "%" $1 $3
		 }


{


-- Hilfsfunktion für Statements wie "int i, i2, i3"
varDeclHelper :: String -> Bool -> [(String, Maybe Exp)] -> [Statement]
varDeclHelper t b [] = []
varDeclHelper t b (x:xs) = [LocalVarDecl t (fst x) (snd x) b] ++ (varDeclHelper t b xs)


-- Hilfsfunktion für Felddeklerationen wie "int i, i2, i3"
fieldDeclHelper :: String -> [Modifier] -> [(String, Maybe Exp)] -> [MemberField]
fieldDeclHelper t m [] = []
fieldDeclHelper t m (x:xs) = [MemberField m t (fst x) (snd x)] ++ (fieldDeclHelper t m xs)

--Hilfsfunktion, macht aus "a.b.c" InstanceVar(InstanceVar(LocalOrFieldVar("a"), "b"), "c")
nameToInstanceVar :: String -> Exp
nameToInstanceVar s = nameToInstanceVar2 (LocalOrFieldVar ((split "." s) !! 0)) (drop 1 (split "." s))

nameToInstanceVar2 :: Exp -> [String] -> Exp
nameToInstanceVar2 e (s:ss) = nameToInstanceVar2 (InstanceVar e s) ss
nameToInstanceVar2 e [] = e

--wird verwendet, um die Expression die beim Casten gelesen wurde als Datentyp zu interpretieren
expToName :: Exp -> String 
expToName (InstanceVar i n) = (expToName i) ++ "." ++ n
expToName (LocalOrFieldVar n) = n
expToName _ = error ""

--prüft grundsätzlich ob verwendete Modifier gültig sind, also ob verschiedene Zugriffsmodifier gleichzeitig verwendet wurden und ob der selbe Modifier mehrfach verwendet wird
checkModifiers :: [Modifier] -> [Modifier]
checkModifiers ms = if (((isJust (elemIndex Public ms)) && (isJust (elemIndex Private ms))) || ((isJust (elemIndex Public ms)) && (isJust (elemIndex Protected ms))) || ((isJust (elemIndex Private ms)) && (isJust (elemIndex Protected ms)))) then error "public, private und protected können nicht zusammen verwendet werden"
		    else if (length (elemIndices Public ms) > 1) then error "public wird mehrfach an der selben Stelle verwendet"
		    else if (length (elemIndices Private ms) > 1) then error "private wird mehrfach an der selben Stelle verwendet"
		    else if (length (elemIndices Protected ms) > 1) then error "protected wird mehrfach an der selben Stelle verwendet"
		    else if (length (elemIndices Abstract ms) > 1) then error "abstract wird mehrfach an der selben Stelle verwendet"
		    else if (length (elemIndices Final ms) > 1) then error "final wird mehrfach an der selben Stelle verwendet"
		    else if (length (elemIndices Static ms) > 1) then error "static wird mehrfach an der selben Stelle verwendet"
		    else ms

--prüft ob die Modifier eines Felds erlaubt sind
checkModifiersField :: [Modifier] -> [Modifier]
checkModifiersField ms = if (isJust (elemIndex Abstract ms)) then error "Ungültiger Modifier bei einem Feld" else ms

--prüft ob die Modifier eines Konstruktor erlaubt sind
checkModifiersConstructor :: [Modifier] -> [Modifier]
checkModifiersConstructor ms = if (isJust (elemIndex Abstract ms) || isJust (elemIndex Final ms) || isJust (elemIndex Static ms)) then error "Ungültiger Modifier bei einem Konstruktor" else ms


--prüft, ob in einem Block unereichbarer Code vorkommt
unreachableCode :: [Statement] -> Bool -> Bool
unreachableCode (s:ss) b = case s of
				Break _ -> if (null ss) then False else unreachableCode ss True
				Return _ -> if (null ss) then False else unreachableCode ss True
				_ -> unreachableCode ss b
unreachableCode [] b = b




--Fehlerbehandlung    
parseError :: [Token] -> a
parseError tokens = if (length tokens > 0) then 
			error $ "Ungültiges Token in aktuellem Kontext, Zeile "
			++ show (getLineFromPosn $ tokenPos (head tokens))  
			++ " Spalte " ++ show (getColumnFromPosn $ tokenPos (head tokens)) 
			++ ": " ++ printToken(head tokens)
			++ "\nEine Auflisten von einigen der möglichen Quellen: \n" 
			++ "-> Strichpunkt vergessen \n"
			++ "-> Statement-Keyword innerhalb einer Expression \n"
			++ "-> Mehr schließende Klammern als öffnende \n"
			++ "-> Operator zwischen Bezeichnern vergessen \n"
			++ "-> Umschließenden Block als Funktionsrumpf vergessen \n"
			
		    else error "Unerwartetes Ende der Eingabe\n-> Möglicherweise werden mehr Klammern geöffnet als geschlossen"




}
