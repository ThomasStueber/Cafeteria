{
module Scanner (alexScanTokens, Token(..), AlexPosn(..)) where
}

%wrapper "posn"

$digit = 0-9
$char = [a-zA-Z]

tokens :-
  
  -- Java Keywords
  abstract				{\p -> \s -> Abstract p }
  assert				{\p -> \s -> Assert p }
  break					{\p -> \s -> Break p }
  byte					{\p -> \s -> Byte p }
  boolean 				{\p -> \s -> Boolean p }
  case					{\p -> \s -> Case p }
  catch					{\p -> \s -> Catch p }
  char 					{\p -> \s -> Char p }
  class					{\p -> \s -> Class p }
  continue				{\p -> \s -> Continue p }
  default				{\p -> \s -> Default p }
  do					{\p -> \s -> Do p }
  double				{\p -> \s -> Double p }
  else					{\p -> \s -> Else p }
  enum					{\p -> \s -> Enum p }
  extends				{\p -> \s -> Extends p }
  final					{\p -> \s -> Final p }
  finally				{\p -> \s -> Finally p }
  float					{\p -> \s -> Float p }
  for					{\p -> \s -> For p }
  if					{\p -> \s -> If p }
  implements				{\p -> \s -> Implements p }
  import				{\p -> \s -> Import p }
  instanceof				{\p -> \s -> InstanceOf p }
  int 					{\p -> \s -> Int p }
  interface				{\p -> \s -> Interface p }
  long 					{\p -> \s -> Long p }
  nativ 				{\p -> \s -> Nativ p }
  new 					{\p -> \s -> New p }
  package 				{\p -> \s -> Package p }
  private 				{\p -> \s -> Private p }
  protected 				{\p -> \s -> Protected p }
  public 				{\p -> \s -> Public p }
  return 				{\p -> \s -> Return p }
  short 				{\p -> \s -> Short p }
  static 				{\p -> \s -> Static p }
  strictfp 				{\p -> \s -> StrictFp p }
  super 				{\p -> \s -> Super p }
  switch 				{\p -> \s -> Switch p }
  synchronized 				{\p -> \s -> Synchronized p }
  this 					{\p -> \s -> This p }
  throw 				{\p -> \s -> Throw p }
  throws 				{\p -> \s -> Throws p }
  transient 				{\p -> \s -> Transient p }
  try 					{\p -> \s -> Try p }
  void 					{\p -> \s -> Void p }
  volatile 				{\p -> \s -> Volatile p }
  while 				{\p -> \s -> While p }
  
  true					{\p -> \s -> BooleanLiteral True p}
  false					{\p -> \s -> BooleanLiteral False p}
  null					{\p -> \s -> NullLiteral p}
  
  
  "+"					{\p -> \s -> Plus p }
  "-"					{\p -> \s -> Minus p }
  "*"					{\p -> \s -> Mul p }
  "/"					{\p -> \s -> Divide p }
  "%"					{\p -> \s -> Modulo p }
  "&"					{\p -> \s -> BitAnd p }
  "|"					{\p -> \s -> BitOr p }
  "^"					{\p -> \s -> BitXor p }
  "~"					{\p -> \s -> BitComplement p }
  "<<"					{\p -> \s -> SignedLeftShift p }
  ">>"					{\p -> \s -> SignedRightShift p }
  ">>>"					{\p -> \s -> UnsignedRightShift p }
  "!"					{\p -> \s -> Not p }
  "++"					{\p -> \s -> PlusPlus p }
  "--"					{\p -> \s -> MinusMinus p }
  "="					{\p -> \s -> Assignment p }
  "+="					{\p -> \s -> PlusAssignment p }
  "-="					{\p -> \s -> MinusAssignment p }
  "*="					{\p -> \s -> MulAssignment p }
  "/="					{\p -> \s -> DivideAssignment p }
  "%="					{\p -> \s -> ModuloAssignment p }
  "<<="					{\p -> \s -> LShiftAssignment p }
  ">>="					{\p -> \s -> RShiftAssignment p }
  ">>>="				{\p -> \s -> UnsignedRShiftAssignment p }
  "&="					{\p -> \s -> AndAssignment p }
  "|="					{\p -> \s -> OrAssignment p }
  "^="					{\p -> \s -> XorAssignment p }
  "&&"					{\p -> \s -> ConditionalAnd p }
  "||"					{\p -> \s -> ConditionalOr p }
  "?"					{\p -> \s -> ConditionalQuestionmark p }
  ":"					{\p -> \s -> ConditionalColon p }
  "=="					{\p -> \s -> Equal p }
  ">="					{\p -> \s -> LessEqual p }
  ">"					{\p -> \s -> LessThan p }
  "<"					{\p -> \s -> GreaterThan p }
  "<="					{\p -> \s -> GreaterEqual p }
  "!="					{\p -> \s -> NotEqual p }
  "("					{\p -> \s -> LeftParenthesis p }
  ")"					{\p -> \s -> RightParenthesis p }
  "["					{\p -> \s -> LeftBracket p }
  "]"					{\p -> \s -> RightBracket p }
  "{"					{\p -> \s -> LeftBraces p }
  "}"					{\p -> \s -> RightBraces p }
  "."					{\p -> \s -> Dot p }
  ";"					{\p -> \s -> Semicolon p }
  
  [$char \_] [$char $digit \_]*				{\p -> \s -> Identifier s p }
  \" [^\"\n]* \"					{\p -> \s -> StringLiteral s p }
  \' [^\'\n] \'						{\p -> \s -> CharLiteral s p }
  \' \\[^\'\n] \'					{\p -> \s -> CharLiteral s p }
  
  -- Java Operators
  
  $white+				;
  [0-9] ([^$white])+			{\p -> \s -> error ("Error in line " ++ (show (getLineFromPosn p)) ++ " (Lexer): " ++ s ++ " is not a lexem! Identifiers can't start with digits.")
						      Error}
  .					{\p -> \s -> error ("Error in line " ++ (show (getLineFromPosn p)) ++ " (Lexer): " ++ s ++ " is a forbidden character.")
						      Error}
  
  

{
getLineFromPosn :: AlexPosn -> Int
getLineFromPosn (AlexPn _ l _) = l


data Token = 
    Abstract AlexPosn			|
    Assert AlexPosn			|
    Boolean AlexPosn			|
    Break AlexPosn			|
    Byte AlexPosn			|
    Case AlexPosn			|
    Catch AlexPosn			|
    Char AlexPosn			|
    Class AlexPosn			|
    Continue AlexPosn			|
    Default AlexPosn			|
    Do AlexPosn				|
    Double AlexPosn			|
    Else AlexPosn			|
    Enum AlexPosn			|
    Extends AlexPosn			|
    Final AlexPosn			|
    Finally AlexPosn			|
    Float AlexPosn			|
    For AlexPosn 			|
    If AlexPosn				|
    Implements AlexPosn			|
    Import AlexPosn			|
    InstanceOf AlexPosn			|
    Int AlexPosn 			|
    Interface AlexPosn			|
    Long AlexPosn			|
    Nativ AlexPosn			|
    New AlexPosn			|
    Package AlexPosn			|
    Private AlexPosn			|
    Protected AlexPosn			|
    Public AlexPosn			|
    Return AlexPosn			|
    Short AlexPosn			|
    Static AlexPosn			|
    StrictFp AlexPosn			|
    Super AlexPosn			|
    Switch AlexPosn			|
    Synchronized AlexPosn		|
    This AlexPosn			|
    Throw AlexPosn			|
    Throws AlexPosn			|
    Transient AlexPosn			|
    Try AlexPosn			|
    Void AlexPosn			|
    Volatile AlexPosn			|
    While AlexPosn			|
    Semicolon AlexPosn			|
    Plus AlexPosn			|
    Minus AlexPosn			|
    Mul AlexPosn			|
    Divide AlexPosn			|
    Modulo AlexPosn			|
    PlusPlus AlexPosn			|
    MinusMinus AlexPosn			|
    Not AlexPosn			|
    Assignment AlexPosn			|
    PlusAssignment AlexPosn		|
    MinusAssignment AlexPosn		|
    MulAssignment AlexPosn		|
    DivideAssignment AlexPosn		|
    ModuloAssignment AlexPosn		|
    AndAssignment AlexPosn		|
    OrAssignment AlexPosn		|
    XorAssignment AlexPosn		|
    LShiftAssignment AlexPosn		|
    RShiftAssignment AlexPosn		|
    UnsignedRShiftAssignment AlexPosn	|
    Equal AlexPosn			|
    NotEqual AlexPosn			|
    GreaterEqual AlexPosn		|
    GreaterThan AlexPosn		|
    LessThan AlexPosn			|
    LessEqual AlexPosn			|
    ConditionalAnd AlexPosn		|
    ConditionalOr AlexPosn		|
    ConditionalQuestionmark AlexPosn 	|
    ConditionalColon AlexPosn 		|
    BitComplement AlexPosn		|
    BitAnd AlexPosn			|
    BitOr AlexPosn			|
    BitXor AlexPosn			|
    SignedLeftShift AlexPosn		|
    SignedRightShift AlexPosn		|
    UnsignedRightShift AlexPosn		|
    Dot AlexPosn			|
    LeftBracket AlexPosn		|
    RightBracket AlexPosn		|
    LeftParenthesis AlexPosn		|
    RightParenthesis AlexPosn		|
    LeftBraces AlexPosn			|
    RightBraces AlexPosn		|
    CharLiteral String AlexPosn		|
    StringLiteral String AlexPosn	|
    IntLiteral Int AlexPosn		|
    FloatLiteral AlexPosn		|
    BooleanLiteral Bool AlexPosn	|
    NullLiteral AlexPosn		|
    Identifier String AlexPosn		|
    Error
    deriving (Eq, Show)



main = do
    print (alexScanTokens "\'c\' true false")
}
