module TypeAdder (addExp, addStatementExp) where
import Abs
import ClassCollector
import qualified Data.Map as M

addExp :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
--literals
addExp _ _ i@(Integer _) = TypedExp i "int"
addExp _ _ c@(Char _)= TypedExp c "char"
addExp _ _ b@(Boolean _)= TypedExp b "boolean"
addExp _ _ s@(String _)= TypedExp s "String"
-- addExp clMap table Null -> take type from variable it is assigned to

addExp clMap table This = TypedExp This (fromJust (M.lookup "this" table))
-- addExp clMap table Super -> ???
addExp clMap table u@(Unary _ _) = addUnary clMap table u
addExp clMap table i@(Infix _ _ _) = addInfix clMap  table i
addExp clMap table inst@(InstanceVar _ _) = addInstVar clMap table inst
addExp clMap table l@(LocalOrFieldVar varName) = if mapResult == Nothing then error ("Variable " ++ (show varName) ++ " nicht definiert") else (TypedExp l (fromJust mapResult))
									     where mapResult = (M.lookup varName table) 
addExp clMap table (StatementExpExp stmtExp) = TypedExp (StatementExpExp typedStmtExp) (statementexptype (typedStmtExp)) where typedStmtExp = addStatementExp clMap table stmtExp
addExp clMap table c@(Cast _ _) = addCast clMap table c 
addExp clMap table instOf@(InstanceOf _ _) = addInstOf clMap table instOf
--addExp clMap table ce@(ConditionalExp cond ifCase elseCase) = addCondExp condExp

addUnary :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
addUnary clMap table u@Unary{unaryop="+", unaryexp=e} = case (exptype typedE) of
															 "int" -> TypedExp (Unary "+" typedE) "int"
															 "char" -> TypedExp (Unary "+" typedE) "int"
															 _ -> error "Schlechter Operand"
														where typedE = addExp clMap table e
addUnary clMap table u@Unary{unaryop="-", unaryexp=e} = case (exptype typedE) of
															 "int" -> TypedExp (Unary "-" typedE) "int"
															 "char" -> TypedExp (Unary "-" typedE) "int"
															 _ -> error "Schlechter Operand"
														where typedE = addExp clMap table e												 												 
addUnary clMap  table u@Unary{unaryop="!", unaryexp=e} = case (exptype typedE) of
															  "boolean" -> TypedExp (Unary "!" typedE) "boolean"
															  _ -> error "Schlechter Operand"
														 where typedE = addExp clMap table e
addUnary clMap  table u@Unary{unaryop="~", unaryexp=e} = case (exptype typedE) of
															 "int" -> TypedExp (Unary "~" typedE) "int"
															 "char" -> TypedExp (Unary "~" typedE) "int"
															 _ -> error "Schlechter Operand"
														 where typedE = addExp clMap table e
												
--
addInfix :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
--logische Operatoren
-- vorsicht::: ((new Cl1()) == null) geht
addInfix clMap table  i@Infix{infixop="==", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of
														   ("char", "int") -> TypedExp (Infix "==" typedL typedR) "boolean"
														   ("int", "char") -> TypedExp (Infix "==" typedL typedR) "boolean"
														   ("boolean", "boolean") -> TypedExp (Infix "==" typedL typedR) "boolean"
														   (_, _) -> if (exptype typedL) == (exptype typedR) then TypedExp (Infix "==" typedL typedR) "boolean" else error "Schlechter Operator" 
													  where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="!=", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of
														   ("char", "int") -> TypedExp (Infix "!=" typedL typedR) "boolean"
														   ("int", "char") -> TypedExp (Infix "!=" typedL typedR) "boolean"
														   ("boolean", "boolean") -> TypedExp (Infix "!=" typedL typedR) "boolean"
														   (_, _) -> if (exptype typedL) == (exptype typedR) then TypedExp (Infix "!=" typedL typedR) "boolean" else error "Schlechter Operator" 
													  where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="&&", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of
														   ("boolean", "boolean") -> TypedExp (Infix "&&" typedL typedR) "boolean"
														   (_, _) -> error "Schlechter Operator"
													  where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="||", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of
													     ("boolean", "boolean") -> TypedExp (Infix "||" typedL typedR) "boolean"
													     (_, _) -> error "Schlechter Operator"
											        where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
--arithmetische Operatoren
addInfix clMap  table  i@Infix{infixop="+", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "+" typedL typedR) "int"
													    ("char", "int") -> TypedExp (Infix "+" typedL typedR) "int"
													    ("int", "char") -> TypedExp (Infix "+" typedL typedR) "int"
													    ("int", "int") -> TypedExp (Infix "+" typedL typedR) "int"
													    --("String", "String") -> TypedExp i "String" ???
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))										
addInfix clMap  table  i@Infix{infixop="-", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "-" typedL typedR) "int"
													    ("char", "int") -> TypedExp (Infix "-" typedL typedR) "int"
													    ("int", "char") -> TypedExp (Infix "-" typedL typedR) "int"
													    ("int", "int") -> TypedExp (Infix "-" typedL typedR) "int"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="*", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "*" typedL typedR) "int"
													    ("char", "int") -> TypedExp (Infix "*" typedL typedR) "int"
													    ("int", "char") -> TypedExp (Infix "*" typedL typedR) "int"
													    ("int", "int") -> TypedExp (Infix "*" typedL typedR) "int"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))										
addInfix clMap  table  i@Infix{infixop="/", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "/" typedL typedR) "int"
													    ("char", "int") -> TypedExp (Infix "/" typedL typedR) "int"
													    ("int", "char") -> TypedExp (Infix "/" typedL typedR) "int"
													    ("int", "int") -> TypedExp (Infix "/" typedL typedR) "int"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="<", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "<" typedL typedR) "boolean"
													    ("char", "int") -> TypedExp (Infix "<" typedL typedR) "boolean"
													    ("int", "char") -> TypedExp (Infix "<" typedL typedR) "boolean"
													    ("int", "int") -> TypedExp (Infix "<" typedL typedR) "boolean"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))	
addInfix clMap  table  i@Infix{infixop=">", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix ">" typedL typedR) "boolean"
													    ("char", "int") -> TypedExp (Infix ">" typedL typedR) "boolean"
													    ("int", "char") -> TypedExp (Infix ">" typedL typedR) "boolean"
													    ("int", "int") -> TypedExp (Infix ">" typedL typedR) "boolean"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))
addInfix clMap  table  i@Infix{infixop="<=", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix "<=" typedL typedR) "boolean"
													    ("char", "int") -> TypedExp (Infix "<=" typedL typedR) "boolean"
													    ("int", "char") -> TypedExp (Infix "<=" typedL typedR) "boolean"
													    ("int", "int") -> TypedExp (Infix "<=" typedL typedR) "boolean"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))	
addInfix clMap  table  i@Infix{infixop=">=", lhs=l, rhs=r} = case ((exptype typedL), (exptype typedR)) of 
													    ("char", "char") -> TypedExp (Infix ">=" typedL typedR) "boolean"
													    ("char", "int") -> TypedExp (Infix ">=" typedL typedR) "boolean"
													    ("int", "char") -> TypedExp (Infix ">=" typedL typedR) "boolean"
													    ("int", "int") -> TypedExp (Infix ">=" typedL typedR) "boolean"
													    (_, _) -> error "Schlechter Operator"
											       where (typedL, typedR) = ((addExp clMap table (l)), (addExp clMap table  (r)))												   

addInstVar :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
addInstVar clMap table (InstanceVar instExp varName) = if (snd result) == Nothing then error "Klasse hat kein solches Feld" else TypedExp (InstanceVar (fst result) varName) (fromJust (snd result)) 
													   where result = (typedInstExp ,(M.lookup varName (classFields (exptype (addExp clMap table instExp)) clMap))) where typedInstExp = (addExp clMap table instExp)
													   


addCast :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
addCast clMap table (Cast newType exp) = case (newType, (exptype typedExp)) of
                                      ("int", "char") -> (TypedExp (Cast newType typedExp) newType)
                                      ("char", "int") -> (TypedExp (Cast newType typedExp) newType)
                                      (_, _) -> if newType == (exptype typedExp) then (TypedExp (Cast newType typedExp) newType) else error "Kann nicht gecasted werden"
                                 where typedExp = addExp clMap table exp									   

{-addInstOf :: (M.Map String Typename) -> Exp -> Exp
addInstOf table (InstanceOf instExp ofName) = case ((exptype typedInstExp), (lookupResult)) of
                                                 ("int", _) -> error "Benötigt Referenz und kein primitive." 
												 ("char", _) -> error "Benötigt Referenz und kein primitive."
												 (_, "int"-}
												 
												 
addInstOf :: ClassMap -> (M.Map String Typename) -> Exp -> Exp
addInstOf clMap table (InstanceOf instExp ofName) = if (M.lookup (exptype typedInstExp) clMap) == Nothing then error "Primitive Datentypen nicht erlaubt"
													else (if ((M.lookup ofName clMap) == Nothing) then error "Klassenname nicht gefunden"
														  else TypedExp (InstanceOf typedInstExp ofName) "boolean")
													where typedInstExp = (addExp clMap table instExp)
														 
												 
												 
												 
												 
addStatementExp :: ClassMap -> (M.Map String Typename) -> StatementExp -> StatementExp	
addStatementExp clMap table a@(Assign _ _ _) = addAssign clMap table a
--addStatementExp table mc@(MethodCall _ _ _) = addMethodCall table mc
--addStatementExp table new@(New _ _) = addNew table new
addStatementExp clMap table pre@(PrefixUnary _ _) = addPrefix clMap table pre
addStatementExp clMap table post@(PostfixUnary _ _) = addPostfix clMap table post



addAssign :: ClassMap -> (M.Map String Typename) -> StatementExp -> StatementExp
--LocalOrFieldVar
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "=") = (if (exptype typedVar) == (exptype typedRHS) then (Assign typedVar typedRHS "=")
                                                          else error "Kann nicht unterschiedliche Typen zuweisen.") 
														  where typedVar = (addExp clMap table var)
														        typedRHS = (addExp clMap table rhs)
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "+=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "-=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "*=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "/=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "%=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "<<=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs ">>=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs ">>>=")=case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "^=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "^=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "|=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "|=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(LocalOrFieldVar _) rhs "&=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "&=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs	
--InastanceVar																
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "=") = if (exptype typedVar) == (exptype typedRHS) then (Assign typedVar typedRHS "=")
                                                        else error "Kann nicht unterschiedliche Typen zuweisen."                                         
														  where typedVar = (addExp clMap table var)
														        typedRHS = (addExp clMap table rhs)
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "+=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "-=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "*=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "/=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "%=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "<<=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs ">>=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs ">>>=")=case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "^=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "^=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "|=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "|=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs
addAssign clMap  table (Assign var@(InstanceVar _  _) rhs "&=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "&=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp clMap table var
														        typedRHS = addExp clMap table rhs	
addAssign _ _ _ = error "Assign nur zu Variable"																

{-addMethodCall :: (M.Map String Typename) -> StatementExp -> StatementExp
addMethodCall table (MethodCall inst methodName params)-}


addPrefix :: ClassMap -> (M.Map String Typename) -> StatementExp -> StatementExp
addPrefix clMap table (PrefixUnary "++" incExp) = case (exptype typedIncExp) of
											   ("int") -> TypedStatementExp (PrefixUnary "++" typedIncExp) "int"
											   ("char") -> TypedStatementExp (PrefixUnary "++" typedIncExp) "int"
											   _ -> error "Kann nicht inkrementiert werden"
										  where typedIncExp = addExp clMap table incExp
addPrefix clMap table (PrefixUnary "--" decExp) = case (exptype typedDecExp) of
											   ("int") -> TypedStatementExp (PrefixUnary "--" typedDecExp) "int"
											   ("char") -> TypedStatementExp (PrefixUnary "--" typedDecExp) "int"
											   _ -> error "Kann nicht dekrementiert werden"
										  where typedDecExp = addExp clMap table decExp
										  
addPostfix :: ClassMap -> (M.Map String Typename) -> StatementExp -> StatementExp
addPostfix clMap table (PostfixUnary "++" incExp) = case (exptype typedIncExp) of
											     ("int") -> TypedStatementExp (PostfixUnary "++" typedIncExp) "int"
											     ("char") -> TypedStatementExp (PostfixUnary "++" typedIncExp) "int"
											     _ -> error "Kann nicht inkrementiert werden"
										    where typedIncExp = addExp clMap table incExp
addPostfix clMap table (PostfixUnary "--" decExp) = case (exptype typedDecExp) of
											     ("int") -> TypedStatementExp (PostfixUnary "--" typedDecExp) "int"
											     ("char") -> TypedStatementExp (PostfixUnary "--" typedDecExp) "int"
											     _ -> error "Kann nicht dekrementiert werden"
										    where typedDecExp = addExp clMap table decExp										  
										  
										  
fromJust :: (Maybe a) -> a
fromJust (Just a) = a
										  
