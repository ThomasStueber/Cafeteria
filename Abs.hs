module Abs where
import Data.Tree
import Data.Maybe

type Typename = String


data ClassDef = ClassDef(Typename, [Modifier], [MemberFunction], [MemberField], [Constructor]) deriving Show

data MemberFunction = MemberFunction(Typename, String, [(Typename, String)], [Modifier], Statement) deriving Show

data MemberField = MemberField([Modifier], Typename, String, Maybe Exp) deriving Show

-- Statements haben in Java keinen Typ
data Statement = Block([Statement])
	       | While(Exp, Statement)                      		-- Abbruchbedingung, Schleifenkörper
	       | If(Exp, Statement, Maybe Statement)       		-- Bedingung, If-Körper, Else-Körper 
	       | Return(Exp)           					-- Ausdruck dessen Ergebniss zurück gegeben werden soll
	       | LocalVarDecl(Typename, String, Maybe Exp)	   	-- Datentyp, Name, Initialisierung
	       | LocalFinalDecl(Typename, String, Exp)	   		-- Datentyp, Name, Initialisierung
	       | For(Statement, Maybe Exp, Maybe StatementExp, Statement)	-- for(Assign, Exp, Assign), Anweisungen
	       | Do(Statement, Exp)					--Anweisungen, Abbruchbedingung
	       | StatementExpStatement(StatementExp)			-- 
	       | EmptyStatement
	       | Break
	       | Continue
	       | Switch(Exp)
	       | Case
	       | Default
	       deriving Show
	  
	  
-- StatementExp ist der Datentyp für Sprachkonstrukte die sowohl Expression als auch Statement sein können
data StatementExp = Assign(Exp, Exp, String)			 	-- Exp1 = Exp2, String ist Operator
	  	  | MethodCall(Exp, String, [Exp])		  	-- Instanz, Funktionsname, Parameter
	  	  | New(Typename, [Exp])				-- Datentyp, Konstruktorparameter
	  	  | PrefixUnary(String, Exp)				-- Operator (++, --), Ausdruck 
		  | PostfixUnary(String, Exp)				-- Operator (++, --), Ausdruck
	  	  | TypedStatementExp(StatementExp, Typename)
		  deriving Show		
		  
-- modifiers
data Modifier = Public
	      | Static
	      | Private
	      | Final
	      deriving Show
	      
	      
	      

-- Expressions
data Exp = This 
	  | Super
	  | Infix(String, Exp, Exp)				-- Operator, linker Operand, rechter Operand
	  | Unary(String, Exp)
	  | InstanceVar(Exp, String)				-- Instanz zu der die Variable gehört, Name der Variablen (siehe Script)
	  | LocalOrFieldVar(String)				-- 
	  | StatementExpExp(StatementExp)
	  | String(String)					-- String Literal
	  | Integer(Int)					-- Int Literal
	  | Boolean(Bool)					-- Boolean Literal
	  | Char(Char)						-- Char Literal
	  | Null						-- Null
	  | Cast(Typename, Exp)					-- Cast
	  | TypedExp(Exp, Typename)
	  deriving Show

	 
	 
data Constructor = Constructor([(Typename, String)], [Modifier], Statement) deriving Show


getTypeFromStatementExp (TypedStatementExp(_, typ)) = typ
getTypeFromExp (TypedExp(_, typ)) = typ






-- hart-gecodedete print-Funktion für abstrakten Syntax

makeNodesMF :: [MemberFunction] -> [Tree String]
makeNodesMF [] = []
makeNodesMF (x:xs) = [memberFunctionToTree(x)] ++ makeNodesMF(xs)

makeNodesMV :: [MemberField] -> [Tree String]
makeNodesMV [] = []
makeNodesMV (x:xs) = [memberFieldToTree(x)] ++ makeNodesMV(xs)

makeNodesStatement :: [Statement] -> [Tree String]
makeNodesStatement [] = []
makeNodesStatement (x:xs) = [statementToTree(x)] ++ makeNodesStatement(xs)

makeNodesExp :: [Exp] -> [Tree String]
makeNodesExp [] = []
makeNodesExp (x:xs) = [expToTree(x)] ++ makeNodesExp(xs)

classDefToTree ::  ClassDef -> (Tree String)
classDefToTree (ClassDef(t, modifiers, fl, vl, cl)) = Node ("ClassDef") [Node ("Klassenname: " ++ t) [], Node ("Modifiers: " ++ (show modifiers)) [], Node "MemberFunctions" (makeNodesMF fl), Node "MemberFields" (makeNodesMV vl)]

memberFunctionToTree ::  MemberFunction -> (Tree String)
memberFunctionToTree (MemberFunction(t, name, parameter, modifier, statement)) = Node "MemberFunction" [Node ("Name: " ++ name) [], Node ("Rückgabedatentyp: " ++ t) [], Node ("Modifier: " ++ (show modifier)) [], Node ("Parameter: " ++ (show parameter)) [], Node "Body" [statementToTree statement]]

memberFieldToTree ::  MemberField -> (Tree String)
memberFieldToTree (MemberField(modifier, t, name, ini)) = Node "MemberField" [Node ("Name: " ++ name) [], Node ("Datentyp" ++ t) [], Node ("Modifiers: " ++ (show modifier)) [], if (isJust ini) then (Node "Initialisierung" [(expToTree (fromJust ini))]) else (Node "Keine Initialisierung" [])]

statementToTree :: Statement -> (Tree String)
statementToTree (Block(sl)) = Node "Block" (makeNodesStatement sl)
statementToTree (While(e, s)) = Node "While" [Node "Schleifenkopf" [(expToTree e)], Node "Schleifenkörper" [(statementToTree s)]]
statementToTree (If(e, ib, eb)) = Node "If" [Node "Bedingung" [(expToTree e)], Node "If-Body" [(statementToTree ib)], if (isJust eb) then (Node "Else" [statementToTree (fromJust eb)]) else (Node "Kein Else-Statement" [])]
statementToTree (Return(e)) = Node "Return" [Node "Return-Expression" [(expToTree e)]]
statementToTree (LocalVarDecl(t, name, ini)) = Node "Dekl. lokaler Variable" [Node ("Name: " ++ name) [], Node ("Datentyp: " ++ t) [], if (isJust ini) then (Node "Initialisierung" [expToTree (fromJust ini)]) else (Node "Variable wird nicht initialisiert" [])]
statementToTree (For(init, e, inc, s)) = Node "For"  [Node "Initialisierung" [(statementToTree (init))], if (isJust e) then (Node "Abbruchbedingung" [(expToTree (fromJust e))]) else (Node "Keine Abbruchbedingung" []), if (isJust inc) then (Node "Inkrementierung" [(statementExpToTree (fromJust inc))]) else (Node "Keine Inkrementierung" []), Node "Schleifenkörper" [(statementToTree s)]]
statementToTree (StatementExpStatement(e)) = statementExpToTree e

statementExpToTree ::  StatementExp -> (Tree String)
statementExpToTree (Assign(e1, e2, op)) = Node "Zuweisung" [Node ("Operator: " ++ op) [], Node "Linke Seite" [expToTree e1], Node "Rechte Seite" [expToTree e2]]
statementExpToTree (New(t, pl)) = Node "New" [Node ("Datentyp: " ++ t) [], Node "Konstruktor-Parameter" (makeNodesExp pl)]
statementExpToTree (MethodCall(inst, name, pl)) = Node "MethodCall" [Node "Instanz" [expToTree inst], Node ("Name: " ++ name) [], Node "Parameter" (makeNodesExp pl)]
statementExpToTree (TypedStatementExp(se,t)) = Node "Typed-StatementExpression" [Node ("Typ: " ++ t) [], statementExpToTree se]
statementExpToTree (PostfixUnary(op,a)) = Node "Postfix-Expression" [Node ("Operator: " ++ op) [], expToTree a]
statementExpToTree (PrefixUnary(op,a)) = Node "Prefix-Expression" [Node ("Operator: " ++ op) [], expToTree a]

expToTree :: Exp -> (Tree String)
expToTree (Infix(op,a,b)) = Node "Infix-Expression" [Node ("Operator: " ++ op) [], expToTree a, expToTree b]
expToTree (InstanceVar(i, name)) = Node "Instanz-Variable" [Node ("Name: " ++ name) [], Node "Instanz" [(expToTree i)]]
expToTree (Cast(t, e)) = Node "Cast" [Node ("Datentyp: " ++ t) [], Node "Ausdruck" [(expToTree e)]]
expToTree (TypedExp(e,t)) = Node "Typed-Expression" [Node ("Typ: " ++ t) [], expToTree e]
expToTree (StatementExpExp(e)) = statementExpToTree e
expToTree e@_ = Node (show e) []


drawAst :: ClassDef -> String
drawAst c = drawTree (classDefToTree(c))

drawExp :: Exp -> String
drawExp e = drawTree (expToTree(e))

drawStatement :: Statement -> String
drawStatement s = drawTree (statementToTree(s))

-- main = putStrLn (drawAst(ClassDef("TestClass", [MemberFunction("int", "testfun1",[],[Public, Static],Block []), MemberFunction("boolean", "testfun2", [], [], Block [])], [], [])))

