module Main  where
import Data.Tree
type Typename = String


data ClassDef = ClassDef(Typename, [MemberFunction], [MemberField], [Constructor]) deriving Show

data MemberFunction = MemberFunction(Typename, String, [(Typename, String)], [Modifier], Statement) deriving Show

data MemberField = MemberField(Modifier, Typename, String) deriving Show

data Statement = Block([Statement])
	       | While(Exp, Statement)                      	-- Abbruchbedingung, Schleifenkörper
	       | If(Exp, Statement, Maybe Statement)       	-- Bedingung, If-Körper, Else-Körper 
	       | Return(Exp)           					   	-- Ausdruck dessen Ergebniss zurück gegeben werden soll
	       | LocalVarDecl(Typename, String)			   	-- Datentyp, Name
	       | For(StatementExp, Exp, StatementExp, Statement)				-- for(Assign, Exp, Assign) Statement
	       deriving Show
	  
	  
data StatementExp = Assign(Exp, Exp, String)			 -- Exp1 = Exp2, String ist modifizierer (+=, -=, etc)
			deriving Show		
data Modifier = Public
	      | Static
	      | Private
	      | Final
	      deriving Show
	      
	      
	      

data Exp = This 
	  | Super
	  | Binary(String, Exp, Exp)							-- Operator, linker Operand, rechter Operand
	  | InstanceVar(Exp, String)							-- Instanz zu der die Variable gehört, Name der Variablen (siehe Script)
	  deriving Show

	 
	 
data Constructor = Constructor([(Typename, String)], [Modifier], Statement) deriving Show












makeNodesMF :: [MemberFunction] -> [Tree String]
makeNodesMF [] = []
makeNodesMF (x:xs) = [memberFunctionToTree(x)] ++ makeNodesMF(xs)

classDefToTree ::  ClassDef -> (Tree String)
classDefToTree (ClassDef(t, fl, vl, cl)) = Node ("ClassDef") [Node ("Klassenname: " ++ t) [], Node "MemberFunctions" (makeNodesMF fl)]

memberFunctionToTree ::  MemberFunction -> (Tree String)
memberFunctionToTree (MemberFunction(t, name, parameter, modifier, statement)) = Node "MemberFunction" [Node ("Name: " ++ name) [], Node ("Rückgabedatentyp" ++ t) [], Node ("Modifier: " ++ (show modifier)) []]

drawAst :: ClassDef -> String
drawAst c = drawTree (classDefToTree(c))

main = putStrLn (drawAst(ClassDef("TestClass", [MemberFunction("int", "testfun1",[],[Public, Static],Block []), MemberFunction("boolean", "testfun2", [], [], Block [])], [], [])))

