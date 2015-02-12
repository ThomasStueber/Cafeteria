module ClassCollector (collectClasses, ClassMap) where
import Abs
import qualified Data.Map as M
import qualified Data.Set as S

											   --Fields					--Constructors						  --Methods
type ClassMap = (M.Map String ((M.Map String Typename), (S.Set [(String, Typename)]), (M.Map String [(String, Typename)])))


collectClasses :: [ClassDef] -> ClassMap 
collectClasses [] = M.empty
collectClasses ls = iterClasses ls M.empty


iterClasses :: [ClassDef] -> ClassMap -> ClassMap
iterClasses [] map = map
iterClasses (cl:cls) map = (iterClasses cls newMap) where newMap = if (isPublic (classmodifiers cl)) then (M.insert (classname cl) massiveMap map)
																   else map
																   where massiveMap = ((getFieldMap (memberfields cl)), (getConsSet (constructors cl)), (getFuncsMap (memberfunctions cl)))
			
--fields			
getFieldMap :: [MemberField] -> (M.Map String Typename)
getFieldMap fields = (getFields fields M.empty)

getFields :: [MemberField] -> (M.Map String Typename) -> (M.Map String Typename)
getFields [] map = map
getFields (field:fields) map = (getFields fields newMap) where newMap = if (isPublic (fieldmodifiers field)) then (M.insert (fieldname field) (fieldtype field) map) 
                                                                        else map


--constructors
getConsSet :: [Constructor] -> (S.Set [(String, Typename)])
getConsSet cons = getConsParams cons S.empty

getConsParams :: [Constructor] -> (S.Set [(String, Typename)]) -> (S.Set [(String, Typename)])
getConsParams [] set = set
getConsParams (con:cons) set = (getConsParams cons newSet) where newSet = if (isPublic (constructorModifiers con)) then (S.insert (getParams (constructorParameters con)) set)
                                                                          else set 
																		

getFuncsMap :: [MemberFunction] -> (M.Map String [(String, Typename)])
getFuncsMap funcs = getFuncs funcs M.empty


getFuncs :: [MemberFunction] -> (M.Map String [(String, Typename)]) -> (M.Map String [(String, Typename)])
getFuncs [] map = map
getFuncs (fun:funs) map = (getFuncs funs newMap) where newMap = if (isPublic (functionmodifiers fun)) then (M.insert (functionname fun) (getParams (formalparameters fun)) map)
                                                                else map

				

getParams :: [(Typename, String)] -> [(String, Typename)]
getParams params = [(switchTuple param) | param <- params]


switchTuple :: (Typename, String) -> (String, Typename)
switchTuple (t, s) = (s, t)					

isPublic :: [Modifier] -> Bool
isPublic [] = True
isPublic (Public:_)= True
isPublic (Private:_) = False
isPublic (_:mods) =  isPublic mods