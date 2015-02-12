module Types (typeCheck) where
import Abs
import TypeAdder
import ClassCollector
import qualified Data.Map as M

typeCheck :: [ClassDef] -> [ClassDef]
typeCheck [] = []
typeCheck (c:cs) = [(getClass c)] ++ (typeCheck cs)

getClass :: ClassDef -> ClassDef
getClass cl = ClassDef (classname cl) (classmodifiers cl) (getMemberFuncs (memberfunctions cl) (snd procFields)) (fst procFields) (getConstructors (constructors cl) (snd procFields)) (getBlock (snd procFields) (classBodyBlocks cl)) where procFields = (fields, (getFieldTable fields M.empty)) where fields = getMemberFields (memberfields cl)

--
-- MemberFunctions
--
getMemberFuncs :: [MemberFunction] -> (M.Map String Typename) -> [MemberFunction]
getMemberFuncs [] _ = []
getMemberFuncs (fun:funs) map = [(getFunction fun map)] ++ (getMemberFuncs funs map)

getFunction :: MemberFunction -> (M.Map String Typename) -> MemberFunction 
getFunction (MemberFunction rType name params mods (Block stLs)) map = MemberFunction rType name params mods (Block (getFuncBody rType map stLs)) where newMap = (getParams params map)
getFunction (MemberFunction rType name params mods stmt) _ = if (isAbstract mods) then (MemberFunction rType name params mods stmt) else error "Methodenkörper muss inerhalb eines Blocks stehen" 

getParams :: [(Typename, String)] -> (M.Map String Typename) -> (M.Map String Typename)
getParams [] map = map
getParams (param:params) map = getParams params newMap where newMap = (putVar (switchTuple param) map)

getFuncBody :: Typename -> (M.Map String Typename) -> [Statement] -> [Statement]
getFuncBody rType map [] = []
getFuncBody rType map (l@(LocalVarDecl _ _ _ _):sts) = [(snd procVarDecl)] ++ (getFuncBody rType (fst procVarDecl) sts) where procVarDecl = getVarDecl map l
getFuncBody rType map (st:sts) = [getStatement True rType map st] ++ (getFuncBody rType map sts)


--
-- MemberFields
--
--fertig
getMemberFields :: [MemberField] -> [MemberField]
getMemberFields [] = []
getMemberFields ((MemberField mods typ name (Just init)):fs) = if typ == (exptype typedInit) then [(MemberField mods typ name (Just typedInit))] ++ (getMemberFields fs) else error "Typen stimmen nicht überein"
																  where typedInit = addExp M.empty init
getMemberFields (mf@(MemberField _ _ _ Nothing):fs)	= [mf] ++ (getMemberFields fs)													  

--fertig
getFieldTable :: [MemberField] -> (M.Map Typename String) -> (M.Map Typename String)
getFieldTable [] map = map
getFieldTable (f:fs) map = getFieldTable fs newMap where newMap = (putVar ((fieldname f), (fieldtype f)) map)



--
-- Constructors
--
getConstructors :: [Constructor] -> (M.Map String Typename) -> [Constructor]
getConstructors [] _ = []
getConstructors (c:cs) map = [(getCon c map)] ++ (getConstructors cs map)

getCon :: Constructor -> (M.Map String Typename) -> Constructor
getCon (Constructor mods name body params) map = Constructor mods name (getStatement True "" newMap body) params where newMap = (getParams params map)




--
-- Classbody Blocks
--
getBlock :: (M.Map String Typename) -> [Statement] -> [Statement]
getBlock _ [] = []
getBlock map ((Block sts):bs) = [(Block (getSingleBlock map sts))] ++ (getBlock map bs)
getBlock map (_:_) = error "Classbody Block muss innerhalb eines Blocks stehen"

getSingleBlock :: (M.Map String Typename) -> [Statement] -> [Statement]
getSingleBlock _ [] = []
getSingleBlock map ((Return _):_) = error "Return in Classbody Block nicht erlaubt"
getSingleBlock map (bl@(Block b):sts) = [Block (getSingleBlock map b)] ++ (getSingleBlock map sts)
getSingleBlock map (l@(LocalVarDecl _ _ _ _):sts) = [(snd procVarDecl)] ++ (getSingleBlock (fst procVarDecl) sts) where procVarDecl = getVarDecl map l 
getSingleBlock map (st:sts) = [(getStatement False "" map st)] ++ (getSingleBlock map sts)


-- 
-- Statements
--
getStatement :: Bool -> Typename -> (M.Map String Typename) -> Statement -> Statement
-- Return
getStatement False _ _ (Return _) = error "Return nicht erlaubt"
getStatement True "void" _ r@(Return Nothing) = r
getStatement True "void" _ (Return _) = error "Return darf keinen Rückgabewert haben"
getStatement True rType map r@(Return (Just exp)) = if (exptype typedExp) == rType then Return (Just typedExp) else error "Typen stimmen nicht überein" 
													where typedExp = addExp map exp
getStatement True rType _ (Return Nothing) = error "Return benötigt Rückgabewert"
-- Block							
getStatement False _ map (Block stLs) = Block (getSingleBlock map stLs)
--getStatement True rType map (Block stLs) = Block (getFuncBody
--more for other blocks
-- While
getStatement return rType map (While cond body) = if (exptype typedCond) == "boolean" then While typedCond (getStatement return rType map body) else error "Condition in while muss Typ boolean haben"
											      where typedCond = addExp map cond
-- If											 
getStatement return rType map (If cond ifSt (Just elseSt)) = if (exptype typedCond) == "boolean" then If typedCond (getStatement return rType map ifSt) (Just (getStatement return rType map elseSt)) else error "Condition in while muss Typ boolean haben"
											                 where typedCond = addExp map cond	
getStatement return rType map (If cond ifSt Nothing) = if (exptype typedCond) == "boolean" then If typedCond (getStatement return rType map ifSt) Nothing else error "Condition in if muss Typ boolean haben"
											           where typedCond = addExp map cond
-- For										   
--getStatement return rType map for@(For _ _ _ _) = getFor return rType map for --implement
-- DO
getStatement return rType map (Do body cond) = if (exptype typedCond) == "boolean" then Do (getStatement return rType map body) typedCond else error "Condition in do muss Typ boolean haben"
											   where typedCond = addExp map cond
-- Switch											 
--getStatement return rType map switch@(Switch _ _) = getSwitch return rType map switch
--getStatement return rType map switchStmt@(SwitchBlockStatement _ _) = getSwitchBlock return rType map switchStmt
--LabeledStatement
getStatement return rType map (LabeledStatement label stmt) = LabeledStatement label (getStatement return rType map stmt)
--StaticBlock
getStatement return rType map (StaticBlock block ) = StaticBlock (getStatement return rType map block)
getStatement return rType map (StatementExpStatement s) = StatementExpStatement (addStatementExp map s) 										 

getNonRecursiveStmt :: (M.Map String Typename) -> Statement -> Statement
--EmptyStatement
getNonRecursiveStmt _ EmptyStatement = EmptyStatement
--
--getNonRecursiveStmt	map  =  	

getVarDecl :: (M.Map String Typename) -> Statement -> ((M.Map String Typename), (Statement))
getVarDecl map v@(LocalVarDecl tName vName (Just init) final) = if (exptype typedInit) == tName then ((putVar (vName, tName) map), (LocalVarDecl tName vName (Just typedInit) final))
																else error "Typen stimmen nicht" 
																where typedInit = addExp map init

getVarDecl map v@(LocalVarDecl tName vName Nothing final) = ((putVar (vName, tName) map), v)
																					 

																					 
--
-- Helper
--
-- put variable in lookup table if it is not yet defined
putVar :: (String, Typename) -> (M.Map String Typename) -> (M.Map String Typename)
putVar (name, typ) map = if (M.lookup (name) map) == Nothing then M.insert name typ map else error ("Variable " ++ name ++ " is already defined")
				   

switchTuple :: (Typename, String) -> (String, Typename)
switchTuple (t, s) = (s, t)	

isAbstract :: [Modifier] -> Bool
isAbstrect [] = False		  
isAbstract (Abstract:mods) = True
isAbstract (_:mods) = isAbstract mods