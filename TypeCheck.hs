module TypeCheck (typeCheck) where
import Abs
import qualified Data.Map as M
import qualified Data.Set as S
import TypeAdder
import ClassCollector


type Table = (M.Map String Typename)

--go through the ClassDefs one by one, giving them the map with all classes with their fields, constructors and methods 
typeCheck :: [ClassDef] -> [ClassDef]
typeCheck ls = [(getClass clMap cl) | cl <- ls] where clMap = collectClasses ls

--get a single class with fields, constructors and methods
getClass :: ClassMap -> ClassDef -> ClassDef
getClass clMap (ClassDef name mods funcs fields cons blocks) = ClassDef name mods (getMemberFuncs clMap (snd procFields) funcs) (fst procFields) (getConstructors clMap (snd procFields) cons) (getBlocks clMap (snd procFields) blocks) where procFields = getMemberFields clMap (M.insert "this" name M.empty) fields

--
-- MemberFields
--

--go through the fields, adding types and the fields to the table
getMemberFields :: ClassMap -> Table -> [MemberField] -> ([MemberField], Table)
getMemberFields clMap table (f:fs) = ([(fst (fst stuff))] ++ (fst (snd stuff)), (M.union (snd (snd stuff)) (snd (fst stuff)))) where stuff = (procF, (getMemberFields clMap (M.union (snd procF) table) fs)) where procF = getField clMap table f
getMemberFields _ map ls = ([], map)
	
--process one field, add the type to the init expression and add the variable to the table			
getField :: ClassMap -> Table -> MemberField -> (MemberField, Table)
getField clMap table (MemberField mods typ name (Just init)) = if (validType clMap typ) && (typ == (exptype typedInit)) then ((MemberField mods typ name (Just typedInit)), (putVar (name, typ) table))
															   else error "Typen nicht gleich"
															   where typedInit = addExp clMap table init--clMap table init
getField clMap table mf@(MemberField mods typ name Nothing) =  if (validType clMap typ) then (mf, (putVar (name, typ) table)) else error "Kein valider Typ"
	

--
-- Constructors
--

getConstructors	:: ClassMap -> Table -> [Constructor] -> [Constructor]
getConstructors clMap table cons = [(getConstructor clMap table con) | con <- cons]

getConstructor :: ClassMap -> Table -> Constructor -> Constructor
getConstructor clMap table (Constructor mods name (Block (ci@(ConstructorInvocation _):sts)) params) = (Constructor mods name (Block ((getConsInvoc clMap newTable ci):(getBody clMap newTable "void" sts))) params) where newTable = getParams clMap table params
getConstructor clMap table (Constructor mods name (Block stLs) params) = (Constructor mods name (Block (getBody clMap newTable "void" stLs)) params) where newTable = getParams clMap table params

getConsInvoc :: ClassMap -> Table -> Statement -> Statement
getConsInvoc clMap table invoc = if (S.member [(exptype exp) | exp <- typedExps] (classConstructors (getThis table) clMap)) then (ConstructorInvocation typedExps) else error "Kein constructor in der Klasse mit diesen Parametern" where typedExps = [(addExp clMap table e) | e <- (constructorInvokParams invoc)]


--
-- MemberFunctions
--

getMemberFuncs :: ClassMap -> Table -> [MemberFunction] -> [MemberFunction]
getMemberFuncs clMap table funcs = [(getFunction clMap table fun) | fun <- funcs]

getFunction :: ClassMap -> Table -> MemberFunction -> MemberFunction
getFunction clMap table (MemberFunction typ name params mods (Block stmtLs)) = (MemberFunction typ name params mods (Block (getBody clMap newTable typ stmtLs))) where newTable = getParams clMap table params
getFunction _ _ _ = error "Classbody muss in einem Block stehen"

getParams :: ClassMap -> Table -> [(Typename, String)] -> Table
getParams _ table [] = table
getParams clMap table (param:params) = (getParams clMap newTable params) where newTable = if (validType clMap (fst param)) then (putVar (snd param, fst param) table) else error "Typ des Parameters nicht valide"


getBody :: ClassMap -> Table -> Typename -> [Statement] -> [Statement]
getBody _ _ _ [] = []
getBody clMap table rType (l@(LocalVarDecl _ _ _ _):sts) = [(fst procVarDecl)] ++ (getBody clMap (snd procVarDecl) rType sts) where procVarDecl = getVarDecl clMap table l
getBody clMap table rType (st:sts) = [(getStatement clMap table True rType st)] ++ (getBody clMap table rType sts)



--
-- ClassBody Blocks
--

getBlocks :: ClassMap -> Table -> [Statement] -> [Statement]
getBlocks clMap table [] = []
getBlocks clMap table ((Block stLs):sts) = [Block (getBlock clMap table stLs)] ++ (getBlocks clMap table sts)
getBlocks _ _ _ = error "ClassBody Blocks müssen in Blocks stehen"

getBlock :: ClassMap -> Table -> [Statement] -> [Statement]
getBlock clMap table stLs = [(getStatement clMap table False "void" st) | st <- stLs]

--
-- Statements
--

getStatement :: ClassMap -> Table -> Bool -> Typename -> Statement -> Statement
--EmptyStatement
getStatement _ _ _ _ EmptyStatement = EmptyStatement
--Return
getStatement _ _ False _ (Return _) = error "Return nicht erlaubt"
getStatement _ _ True "void" r@(Return Nothing) = r
getStatement _ _ True "void" (Return _) = error "Return darf keinen Rückgabewert haben"
getStatement clMap table True rType r@(Return (Just exp)) = if (exptype typedExp) == rType then Return (Just typedExp) else error "Typen stimmen nicht überein" 
													where typedExp = addExp clMap table exp
getStatement _ _ True rType (Return Nothing) = error "Return benötigt Rückgabewert"
--Block
getStatement clMap table False _ (Block stLs) = Block (getBlock clMap table stLs)
getStatement clMap table True rType (Block stLs) = Block (getBody clMap table rType stLs)
--If-Else
getStatement clMap table return rType (If cond ifBody (Just elseBody)) = if (exptype typedCond) == "boolean" then (If typedCond (getStatement clMap table return rType ifBody) (Just (getStatement clMap table return rType elseBody))) else error "Condition muss vom Typ boolean sein" where typedCond = addExp clMap table cond
getStatement clMap table return rType (If cond ifBody Nothing) = if (exptype typedCond) == "boolean" then (If typedCond (getStatement clMap table return rType ifBody) Nothing) else error "Condition muss vom Typ boolean sein" where typedCond = addExp clMap table cond
--Loops
getStatement clMap table return rType (While cond body) = if (exptype typedCond) == "boolean" then While typedCond (getStatementInLoop clMap table return rType body) else error "Condition in while muss Typ boolean haben"
														  where typedCond = addExp clMap table cond														  
getStatement clMap table return rType (For (forInit@(LocalVarDecl _ _ (Just init) _):[]) (Just cond) (incr:[]) body) = For ((fst procForInit):[]) (let typedCond = (addExp clMap (snd procForInit) cond) 
																																				   in (if (exptype typedCond == "boolean") then Just typedCond 
																																					   else error ("Condition " ++(show typedCond) ++" muss Typ boolean haben"))) ((addStatementExp clMap (snd procForInit) incr):[]) (getStatementInLoop clMap (snd procForInit) return rType body) where procForInit = getVarDecl clMap table forInit
getStatement clMap table return rType (For (forInit@(LocalVarDecl _ _ (Just init) _):[]) Nothing (incr:[]) body) = For ((fst procForInit):[]) Nothing ((addStatementExp clMap (snd procForInit) incr):[]) (getStatementInLoop clMap (snd procForInit) return rType body) where procForInit = getVarDecl clMap table forInit
getStatement clMap table return rType (For ((LocalVarDecl _ _ Nothing _):[]) _ _ _) = error "Loop control Variable muss initialisiert werden" 
getStatement clMap table return rType (Do body cond) = if (exptype typedCond) == "boolean" then (Do (getStatementInLoop clMap table return rType body) typedCond) else error "Condition in do-while muss Typ boolean haben"
													   where typedCond = addExp clMap table cond
--StatementExpStatement
getStatement clMap table _ _ (StatementExpStatement stmtExp) = StatementExpStatement (addStatementExp clMap table stmtExp)

--LabeledStatement
getStatement clMap table return rType (LabeledStatement label stmt) = LabeledStatement label (getStatement clMap table return rType stmt)
	
--Statements in the wrong place	
getStatement _ _ _ _ (ConstructorInvocation _) = error "Constructor invocation nicht am Anfang von Constructor"
getStatement _ _ _ _ (Continue _) = error "Continue außerhalb von Schleife"
getStatement _ _ _ _ (Break _) = error "Break außerhalb von Schleife"

														  
getStatementInLoop :: ClassMap -> Table -> Bool -> Typename -> Statement -> Statement
getStatementInLoop clMap table return rType (Block stLs) = Block [(getStatementInLoop clMap table return rType st) | st <- stLs]
getStatementInLoop _ _ _ _ (Continue label) = Continue label														  
getStatementInLoop _ _ _ _ (Break label) = Break label
getStatementInLoop clMap table return rType (If cond ifBody (Just elseBody)) = if (exptype typedCond) == "boolean" then (If typedCond (getStatementInLoop clMap table return rType ifBody) (Just (getStatementInLoop clMap table return rType elseBody))) else error "Condition muss vom Typ boolean sein" where typedCond = addExp clMap table cond
getStatementInLoop clMap table return rType (If cond ifBody Nothing) = if (exptype typedCond) == "boolean" then (If typedCond (getStatementInLoop clMap table return rType ifBody) Nothing) else error "Condition muss vom Typ boolean sein" where typedCond = addExp clMap table cond
getStatementInLoop clMap table return rType (LabeledStatement label stmt) = LabeledStatement label (getStatementInLoop clMap table return rType stmt)
getStatementInLoop clMap table return rType stmt = getStatement clMap table return rType stmt


getVarDecl :: ClassMap -> Table -> Statement -> (Statement, Table)
getVarDecl clMap table (LocalVarDecl typ name (Just init) final) = if (validType clMap typ) then (if (typ == (exptype typedInit)) then ((LocalVarDecl typ name (Just typedInit) final), (putVar (name, typ) table)) else error "Typen stimmen nicht überein") else error "Kein valider Typ" where typedInit = addExp clMap table init
getVarDecl clMap table l@(LocalVarDecl typ name Nothing final) = if (validType clMap typ) then (l, (putVar (name, typ) table)) else error "Kein valider Typ"

--
-- Helper
--

validType :: ClassMap -> Typename -> Bool
validType _ "char" = True
validType _ "int" = True
validType _ "boolean" = True
validType clMap typ = if (M.lookup typ clMap) == Nothing then False else True

putVar :: (String, Typename) -> Table -> Table
putVar (name, typ) table = if (M.lookup (name) table) == Nothing then M.insert name typ table else error ("Variable " ++ name ++ " is already defined")

getThis :: Table -> String
getThis table = fromJust (M.lookup "this" table)

fromJust :: (Maybe a) -> a
fromJust (Just a) = a