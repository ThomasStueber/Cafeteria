{
module Scanner (alexScanTokens, Token(..), AlexPosn(..)) where
import Data.Char
import Numeric
import Data.String.Utils
}

%wrapper "posn"

$digit = 0-9
$hexdigit = [0-9A-Fa-f]
$bindigit = [0-1]
$octaldigit = [0-7]
$char = [a-zA-Z]

tokens :-
  
  -- Java Keywords
  abstract				{\p -> \s -> AbstractToken p }
  assert				{\p -> \s -> AssertToken p }
  break					{\p -> \s -> BreakToken p }
  byte					{\p -> \s -> ByteToken p }
  boolean 				{\p -> \s -> BooleanToken p }
  case					{\p -> \s -> CaseToken p }
  catch					{\p -> \s -> CatchToken p }
  char 					{\p -> \s -> CharToken p }
  class					{\p -> \s -> ClassToken p }
  continue				{\p -> \s -> ContinueToken p }
  default				{\p -> \s -> DefaultToken p }
  do					{\p -> \s -> DoToken p }
  double				{\p -> \s -> DoubleToken p }
  else					{\p -> \s -> ElseToken p }
  enum					{\p -> \s -> EnumToken p }
  extends				{\p -> \s -> ExtendsToken p }
  final					{\p -> \s -> FinalToken p }
  finally				{\p -> \s -> FinallyToken p }
  float					{\p -> \s -> FloatToken p }
  for					{\p -> \s -> ForToken p }
  if					{\p -> \s -> IfToken p }
  implements				{\p -> \s -> ImplementsToken p }
  import				{\p -> \s -> ImportToken p }
  instanceof				{\p -> \s -> InstanceOfToken p }
  int 					{\p -> \s -> IntToken p }
  interface				{\p -> \s -> InterfaceToken p }
  long 					{\p -> \s -> LongToken p }
  nativ 				{\p -> \s -> NativToken p }
  new 					{\p -> \s -> NewToken p }
  package 				{\p -> \s -> PackageToken p }
  private 				{\p -> \s -> PrivateToken p }
  protected 				{\p -> \s -> ProtectedToken p }
  public 				{\p -> \s -> PublicToken p }
  return 				{\p -> \s -> ReturnToken p }
  short 				{\p -> \s -> ShortToken p }
  static 				{\p -> \s -> StaticToken p }
  strictfp 				{\p -> \s -> StrictFpToken p }
  super 				{\p -> \s -> SuperToken p }
  switch 				{\p -> \s -> SwitchToken p }
  synchronized 				{\p -> \s -> SynchronizedToken p }
  this 					{\p -> \s -> ThisToken p }
  throw 				{\p -> \s -> ThrowToken p }
  throws 				{\p -> \s -> ThrowsToken p }
  transient 				{\p -> \s -> TransientToken p }
  try 					{\p -> \s -> TryToken p }
  void 					{\p -> \s -> VoidToken p }
  volatile 				{\p -> \s -> VolatileToken p }
  while 				{\p -> \s -> WhileToken p }
  
  true					{\p -> \s -> BooleanLiteralToken True p}
  false					{\p -> \s -> BooleanLiteralToken False p}
  null					{\p -> \s -> NullToken p}
  
  
  "+"					{\p -> \s -> PlusToken p }
  "-"					{\p -> \s -> MinusToken p }
  "*"					{\p -> \s -> MulToken p }
  "/"					{\p -> \s -> DivideToken p }
  "%"					{\p -> \s -> ModuloToken p }
  "&"					{\p -> \s -> BitAndToken p }
  "|"					{\p -> \s -> BitOrToken p }
  "^"					{\p -> \s -> BitXorToken p }
  "~"					{\p -> \s -> BitComplementToken p }
  "<<"					{\p -> \s -> SignedLeftShiftToken p }
  ">>"					{\p -> \s -> SignedRightShiftToken p }
  ">>>"					{\p -> \s -> UnsignedRightShiftToken p }
  "!"					{\p -> \s -> NotToken p }
  "++"					{\p -> \s -> PlusPlusToken p }
  "--"					{\p -> \s -> MinusMinusToken p }
  "="					{\p -> \s -> AssignmentToken p }
  "+="					{\p -> \s -> PlusAssignmentToken p }
  "-="					{\p -> \s -> MinusAssignmentToken p }
  "*="					{\p -> \s -> MulAssignmentToken p }
  "/="					{\p -> \s -> DivideAssignmentToken p }
  "%="					{\p -> \s -> ModuloAssignmentToken p }
  "<<="					{\p -> \s -> LShiftAssignmentToken p }
  ">>="					{\p -> \s -> RShiftAssignmentToken p }
  ">>>="				{\p -> \s -> UnsignedRShiftAssignmentToken p }
  "&="					{\p -> \s -> AndAssignmentToken p }
  "|="					{\p -> \s -> OrAssignmentToken p }
  "^="					{\p -> \s -> XorAssignmentToken p }
  "&&"					{\p -> \s -> ConditionalAndToken p }
  "||"					{\p -> \s -> ConditionalOrToken p }
  "?"					{\p -> \s -> ConditionalQuestionmarkToken p }
  ":"					{\p -> \s -> ConditionalColonToken p }
  "=="					{\p -> \s -> EqualToken p }
  ">="					{\p -> \s -> LessEqualToken p }
  ">"					{\p -> \s -> LessThanToken p }
  "<"					{\p -> \s -> GreaterThanToken p }
  "<="					{\p -> \s -> GreaterEqualToken p }
  "!="					{\p -> \s -> NotEqualToken p }
  "("					{\p -> \s -> LeftParenthesisToken p }
  ")"					{\p -> \s -> RightParenthesisToken p }
  "["					{\p -> \s -> LeftBracketToken p }
  "]"					{\p -> \s -> RightBracketToken p }
  "{"					{\p -> \s -> LeftBracesToken p }
  "}"					{\p -> \s -> RightBracesToken p }
  "."					{\p -> \s -> DotToken p }
  ";"					{\p -> \s -> SemicolonToken p }
  ","					{\p -> \s -> CommaToken p }

  (0[bB]$bindigit | 0[bB](($bindigit+(\_)*)+$bindigit))	{\p -> \s -> IntLiteralToken (binToInt (drop 2 (replace "_" "" s))) p}
  (0[xX]$hexdigit | 0[xX](($hexdigit+(\_)*)+$hexdigit))	{\p -> \s -> IntLiteralToken (fst(head(readHex (drop 2 (replace "_" "" s))))) p}
  (0$octaldigit | 0(($octaldigit+(\_)*)+$octaldigit))	{\p -> \s -> IntLiteralToken (fst(head(readOct (drop 1 (replace "_" "" s))))) p}
  ($digit | ($digit+(\_)*)+$digit)			{\p -> \s -> IntLiteralToken (read (replace "_" "" s)) p}
  
  [$char \_] [$char $digit \_]*				{\p -> \s -> IdentifierToken s p }
  \" (((\\\")?|(\\t)?|(\\b)?|(\\n)?|(\\r)?|(\\f)?|(\\0)?|(\\\')?|(\\\\)?|(\\u$hexdigit$hexdigit$hexdigit$hexdigit))*[^\\\"\n\r\f\'\b\t]?((\\\")?|(\\t)?|(\\b)?|(\\n)?|(\\r)?|(\\f)?|(\\\')?|(\\0)?|(\\\\)?|(\\u$hexdigit$hexdigit$hexdigit$hexdigit))*)* \"					{\p -> \s -> StringLiteralToken (resolveEscapeSequences ((take ((length s)-2) (drop 1 s)),0,"")) p }
  \' [^\\\"\n\r\f\'\b\t] \'				{\p -> \s -> CharLiteralToken (s !! 1) p }
  \' \\\' \'						{\p -> \s -> CharLiteralToken '\'' p }
  \' \\\" \'						{\p -> \s -> CharLiteralToken '\"' p }
  \' \\\\ \'						{\p -> \s -> CharLiteralToken '\\' p }
  \' \\n \'						{\p -> \s -> CharLiteralToken '\n' p }
  \' \\r \'						{\p -> \s -> CharLiteralToken '\r' p }
  \' \\t \'						{\p -> \s -> CharLiteralToken '\t' p }
  \' \\b \'						{\p -> \s -> CharLiteralToken '\b' p }
  \' \\f \'						{\p -> \s -> CharLiteralToken '\f' p }
  \' \\0 \'						{\p -> \s -> CharLiteralToken '\0' p }
  \' \\u$hexdigit$hexdigit$hexdigit$hexdigit \'		{\p -> \s -> CharLiteralToken  (chr (fst(head (readHex (take 4 (drop 3 s)))))) p }
  
  
  -- Java Operators
  
  $white+				;
  "//".*				;
  "/*"([\n]|.)*"*/"			;
  
  [0-9] ([A-Za-z\_])+			{\p -> \s -> error ("Error in line " ++ (show (getLineFromPosn p)) ++ " (Lexer): " ++ s ++ " is not a lexem! Identifiers can't start with digits.")
						      ErrorToken}
						      
  
  
  .					{\p -> \s -> error ("Error in line " ++ (show (getLineFromPosn p)) ++ " (Lexer): " ++ s ++ " is a forbidden character.")
						      ErrorToken}
  
  

{
getLineFromPosn :: AlexPosn -> Int
getLineFromPosn (AlexPn _ l _) = l


data Token = 
    AbstractToken AlexPosn			|
    AssertToken AlexPosn			|
    BooleanToken AlexPosn			|
    BreakToken AlexPosn				|
    ByteToken AlexPosn				|
    CaseToken AlexPosn				|
    CatchToken AlexPosn				|
    CharToken AlexPosn				|
    ClassToken AlexPosn				|
    ContinueToken AlexPosn			|
    DefaultToken AlexPosn			|
    DoToken AlexPosn				|
    DoubleToken AlexPosn			|
    ElseToken AlexPosn				|
    EnumToken AlexPosn				|
    ExtendsToken AlexPosn			|
    FinalToken AlexPosn				|
    FinallyToken AlexPosn			|
    FloatToken AlexPosn				|
    ForToken AlexPosn 				|
    IfToken AlexPosn				|
    ImplementsToken AlexPosn			|
    ImportToken AlexPosn			|
    InstanceOfToken AlexPosn			|
    IntToken AlexPosn 				|
    InterfaceToken AlexPosn			|
    LongToken AlexPosn				|
    NativToken AlexPosn				|
    NewToken AlexPosn				|
    PackageToken AlexPosn			|
    PrivateToken AlexPosn			|
    ProtectedToken AlexPosn			|
    PublicToken AlexPosn			|
    ReturnToken AlexPosn			|
    ShortToken AlexPosn				|
    StaticToken AlexPosn			|
    StrictFpToken AlexPosn			|
    SuperToken AlexPosn				|
    SwitchToken AlexPosn			|
    SynchronizedToken AlexPosn			|
    ThisToken AlexPosn				|
    ThrowToken AlexPosn				|
    ThrowsToken AlexPosn			|
    TransientToken AlexPosn			|
    TryToken AlexPosn				|
    VoidToken AlexPosn				|
    VolatileToken AlexPosn			|
    WhileToken AlexPosn				|
    SemicolonToken AlexPosn			|
    CommaToken AlexPosn				|
    PlusToken AlexPosn				|
    MinusToken AlexPosn				|
    MulToken AlexPosn				|
    DivideToken AlexPosn			|
    ModuloToken AlexPosn			|
    PlusPlusToken AlexPosn			|
    MinusMinusToken AlexPosn			|
    NotToken AlexPosn				|
    AssignmentToken AlexPosn			|
    PlusAssignmentToken AlexPosn		|
    MinusAssignmentToken AlexPosn		|
    MulAssignmentToken AlexPosn			|
    DivideAssignmentToken AlexPosn		|
    ModuloAssignmentToken AlexPosn		|
    AndAssignmentToken AlexPosn			|
    OrAssignmentToken AlexPosn			|
    XorAssignmentToken AlexPosn			|
    LShiftAssignmentToken AlexPosn		|
    RShiftAssignmentToken AlexPosn		|
    UnsignedRShiftAssignmentToken AlexPosn	|
    EqualToken AlexPosn				|
    NotEqualToken AlexPosn			|
    GreaterEqualToken AlexPosn			|
    GreaterThanToken AlexPosn			|
    LessThanToken AlexPosn			|
    LessEqualToken AlexPosn			|
    ConditionalAndToken AlexPosn		|
    ConditionalOrToken AlexPosn			|
    ConditionalQuestionmarkToken AlexPosn 	|
    ConditionalColonToken AlexPosn 		|
    BitComplementToken AlexPosn			|
    BitAndToken AlexPosn			|
    BitOrToken AlexPosn				|
    BitXorToken AlexPosn			|
    SignedLeftShiftToken AlexPosn		|
    SignedRightShiftToken AlexPosn		|
    UnsignedRightShiftToken AlexPosn		|
    DotToken AlexPosn				|
    LeftBracketToken AlexPosn			|
    RightBracketToken AlexPosn			|
    LeftParenthesisToken AlexPosn		|
    RightParenthesisToken AlexPosn		|
    LeftBracesToken AlexPosn			|
    RightBracesToken AlexPosn			|
    CharLiteralToken Char AlexPosn		|
    StringLiteralToken String AlexPosn		|
    IntLiteralToken Int AlexPosn		|
    FloatLiteralToken AlexPosn			|
    BooleanLiteralToken Bool AlexPosn		|
    NullToken AlexPosn				|
    IdentifierToken String AlexPosn		|
    ErrorToken
    deriving (Eq, Show)


resolveEscapeSequences :: ([Char], Int, String) -> [Char]
resolveEscapeSequences ([], state, temp) = if (state == 0) then [] else (error "Falsche Escape-Sequence")
resolveEscapeSequences (c:cs, state, temp) = 	if (state == 0 && c /= '\\') then ([c] ++ (resolveEscapeSequences (cs, 0, ""))) 
					else if (state == 0 && c == '\\') then (resolveEscapeSequences (cs,1,""))
					else if (state == 1 && c == 'n') then ("\n" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == 'r') then ("\r" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == 't') then ("\t" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == 'f') then ("\f" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == 'b') then ("\b" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == '\'') then ("\'" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == '\"') then ("\"" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == '\\') then ("\\" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == '\0') then ("\0" ++ (resolveEscapeSequences (cs,0,""))) 
					else if (state == 1 && c == 'u') then (resolveEscapeSequences (cs,2,""))
					else if (state == 2 && (isHexDigit c)) then (resolveEscapeSequences (cs,3,[c]))
					else if (state == 3 && (isHexDigit c)) then (resolveEscapeSequences (cs,4,temp ++ [c]))
					else if (state == 4 && (isHexDigit c)) then (resolveEscapeSequences (cs,5,temp ++ [c]))
					else if (state == 5 && (isHexDigit c)) then [(chr (fst(head (readHex (temp ++ [c])))))] ++ (resolveEscapeSequences (cs,0,""))
					else error "Irgendwas lief falsch, das hier sollte man nie sehen können"
					

					
binToNum :: Char -> Int
binToNum '0' = 0
binToNum '1' = 1
binToNum _ = -1

binToInt :: [Char] -> Int
binToInt s = binToInt2 ((reverse s),0)

binToInt2 :: ([Char], Int) -> Int
binToInt2 (s:ss, e) = (binToNum s) * (2 ^ (e)) + binToInt2(ss, e+1)
binToInt2 ([], e) = 0

main = do
    print (alexScanTokens "\'c\' true false")
}
