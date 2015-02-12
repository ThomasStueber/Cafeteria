module TypeAdder (addExp, addStatementExp) where
import Abs
import qualified Data.Map as M

addExp :: (M.Map String Typename) -> Exp -> Exp
--literals
addExp _ i@(Integer _) = TypedExp i "int"
addExp _ c@(Char _)= TypedExp c "char"
addExp _ b@(Boolean _)= TypedExp b "boolean"
addExp _ s@(String _)= TypedExp s "String"
-- addExp map Null -> take type from variable it is assigned to

-- addExp map This -> put classname into list as (this, classname)
-- addExp map Super -> ???
addExp map u@(Unary _ _) = addUnary map u
addExp map i@(Infix _ _ _) = addInfix map i
addExp map inst@(InstanceVar _ _) = String "NOT IMPLEMENTED" --addInstVar map inst
addExp map l@(LocalOrFieldVar varName) = if mapResult == Nothing then error "Variable nicht definiert" else (TypedExp l (fromJust mapResult))
									     where mapResult = (M.lookup varName map) 
addExp map (StatementExpExp stmtExp) = TypedExp (StatementExpExp typedStmtExp) (statementexptype (typedStmtExp)) where typedStmtExp = addStatementExp map stmtExp
--addExp map c@(Cast _ _) = addCast c 
--addExp map instOf@(InstanceOf _ _) = addInstOf instOf
--addExp map ce@(ConditionalExp cond ifCase elseCase) = addCondExp condExp

addUnary :: (M.Map String Typename) -> Exp -> Exp
addUnary map u@Unary{unaryop="+", unaryexp=e} = case eType of
												     "int" -> TypedExp u "int"
												     "char" -> TypedExp u "int"
												     _ -> error "Schlechter Operand"
											    where eType = exptype (addExp map e)
addUnary map u@Unary{unaryop="-", unaryexp=e} = case eType of
												     "int" -> TypedExp u "int"
												     "char" -> TypedExp u "int"
												     _ -> error "Schlechter Operand"
											    where eType = exptype (addExp map e)												 												 
addUnary map u@Unary{unaryop="!", unaryexp=e} = case eType of
												     "boolean" -> TypedExp u "boolean"
												     _ -> error "Schlechter Operand"
											    where eType = exptype (addExp map e)
addUnary map u@Unary{unaryop="~", unaryexp=e} = case eType of
												     "int" -> TypedExp u "int"
												     "char" -> TypedExp u "int"
												     _ -> error "Schlechter Operand"
											    where eType = exptype (addExp map e)
												
--
addInfix :: (M.Map String Typename) -> Exp -> Exp
--logische Operatoren
-- vorsicht::: ((new Cl1()) == null) geht
addInfix map  i@Infix{infixop="==", lhs=l, rhs=r} = case (lType, rType) of
													     ("char", "int") -> TypedExp i "boolean"
													     ("int", "char") -> TypedExp i "boolean"
													     (_, _) -> if lType == rType then TypedExp i "boolean" else error "Schlechter Operator" 
											        where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
addInfix map  i@Infix{infixop="!=", lhs=l, rhs=r} = case (lType, rType) of
													     ("char", "int") -> TypedExp i "boolean"
													     ("int", "char") -> TypedExp i "boolean"
													     (_, _) -> if lType == rType then TypedExp i "boolean" else error "Schlechter Operator" 
											        where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
addInfix map  i@Infix{infixop="&&", lhs=l, rhs=r} = case (lType, rType) of
													     ("boolean", "boolean") -> TypedExp i "boolean"
													     (_, _) -> error "Schlechter Operator"
											        where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
addInfix map  i@Infix{infixop="||", lhs=l, rhs=r} = case (lType, rType) of
													     ("boolean", "boolean") -> TypedExp i "boolean"
													     (_, _) -> error "Schlechter Operator"
											        where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
--arithmetische Operatoren
addInfix map  i@Infix{infixop="+", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    --("String", "String") -> TypedExp i "String" ???
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))										
addInfix map  i@Infix{infixop="-", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
addInfix map  i@Infix{infixop="*", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))										
addInfix map  i@Infix{infixop="/", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))
addInfix map  i@Infix{infixop="<", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))	
addInfix map  i@Infix{infixop=">", lhs=l, rhs=r} = case (lType, rType) of 
													    ("char", "char") -> TypedExp i "int"
													    ("char", "int") -> TypedExp i "int"
													    ("int", "char") -> TypedExp i "int"
													    ("int", "int") -> TypedExp i "int"
													    (_, _) -> error "Schlechter Operator"
											       where (lType, rType) = (exptype (addExp map  (l)), exptype (addExp map  (r)))

--addInstVar :: (M.Map String Typename) -> Exp -> Exp
--addInstVar	map (InstanceVar instExp varName) = InstanceVar (addExp map instExp) (M.lookup 									   


addCast :: (M.Map String Typename) -> Exp -> Exp
{-addCast map (Cast newType exp) = if newType == (exptype typedExp) then (TypedExp (Case newType typedExp) newType)
                                 else (case (newType, (exptype typedExp)) of
							               ("int", "char") -> TypedExp (Cast newType typedExp) newType
									       ("char", "int") -> TypedExp (Cast newType typedExp) newType
                                           (_, _) -> error "Kann nicht gecasted werden")
                                 where typedExp = (addExp exp)-}
addCast map (Cast newType exp) = case (newType, (exptype typedExp)) of
                                      ("int", "char") -> (TypedExp (Cast newType typedExp) newType)
                                      ("char", "int") -> (TypedExp (Cast newType typedExp) newType)
                                      (_, _) -> error "Kann nicht gecasted werden"
                                 where typedExp = addExp map exp									   

{-addInstOf :: (M.Map String Typename) -> Exp -> Exp
addInstOf map (InstanceOf instExp ofName) = case ((exptype typedInstExp), (lookupResult)) of
                                                 ("int", _) -> error "Benötigt Referenz und kein primitive." 
												 ("char", _) -> error "Benötigt Referenz und kein primitive."
												 (_, "int"-}
												 
												 
												 
												 
												 
												 
												 
addStatementExp :: (M.Map String Typename) -> StatementExp -> StatementExp	
addStatementExp map a@(Assign _ _ _) = addAssign map a
--addStatementExp map mc@(MethodCall _ _ _) = addMethodCall map mc
--addStatementExp map new@(New _ _) = addNew map new
addStatementExp map pre@(PrefixUnary _ _) = addPrefix map pre
addStatementExp map post@(PostfixUnary _ _) = addPostfix map post



addAssign :: (M.Map String Typename) -> StatementExp -> StatementExp
--LocalOrFieldVar
addAssign map (Assign var@(LocalOrFieldVar _) rhs "=") = (if (exptype typedVar) == (exptype typedRHS) then (Assign typedVar typedRHS "=")
                                                          else error "Kann nicht unterschiedliche Typen zuweisen.") 
														  where typedVar = (addExp map var)
														        typedRHS = (addExp map rhs)
addAssign map (Assign var@(LocalOrFieldVar _) rhs "+=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "-=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "*=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "/=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "%=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "<<=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs ">>=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs ">>>=")=case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "^=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "^=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "|=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "|=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(LocalOrFieldVar _) rhs "&=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "&=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs	
--InastanceVar																
addAssign map (Assign var@(InstanceVar _  _) rhs "=") = if (exptype typedVar) == (exptype typedRHS) then (Assign typedVar typedRHS "=")
                                                        else error "Kann nicht unterschiedliche Typen zuweisen."                                         
														  where typedVar = (addExp map var)
														        typedRHS = (addExp map rhs)
addAssign map (Assign var@(InstanceVar _  _) rhs "+=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "+=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "-=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "-=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "*=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "*=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "/=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "/=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "%=") = case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "%=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "<<=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS "<<=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs ">>=")= case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs ">>>=")=case ((exptype typedVar),(exptype typedRHS)) of
															   ("char", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "char")
															   ("int", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("int", "char") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   ("char", "int") -> (TypedStatementExp (Assign typedVar typedRHS ">>>=") "int")
															   (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "^=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "^=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "|=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "|=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs
addAssign map (Assign var@(InstanceVar _  _) rhs "&=")=case ((exptype typedVar),(exptype typedRHS)) of
															 ("boolean", "boolean") -> (TypedStatementExp (Assign typedVar typedRHS "&=") "boolean")
															 (_, _) -> error "Zuweisung von diesen Typen nicht erlaubt."
														  where typedVar = addExp map var
														        typedRHS = addExp map rhs	
addAssign _ _ = error "Assign nur zu Variable"																

{-addMethodCall :: (M.Map String Typename) -> StatementExp -> StatementExp
addMethodCall map (MethodCall inst methodName params)-}


addPrefix :: (M.Map String Typename) -> StatementExp -> StatementExp
addPrefix map (PrefixUnary "++" incExp) = case (exptype typedIncExp) of
											   ("int") -> TypedStatementExp (PrefixUnary "++" typedIncExp) "int"
											   ("char") -> TypedStatementExp (PrefixUnary "++" typedIncExp) "int"
											   _ -> error "Kann nicht inkrementiert werden"
										  where typedIncExp = addExp map incExp
addPrefix map (PrefixUnary "--" decExp) = case (exptype typedDecExp) of
											   ("int") -> TypedStatementExp (PrefixUnary "--" typedDecExp) "int"
											   ("char") -> TypedStatementExp (PrefixUnary "--" typedDecExp) "int"
											   _ -> error "Kann nicht dekrementiert werden"
										  where typedDecExp = addExp map decExp
										  
addPostfix :: (M.Map String Typename) -> StatementExp -> StatementExp
addPostfix map (PostfixUnary "++" incExp) = case (exptype typedIncExp) of
											     ("int") -> TypedStatementExp (PostfixUnary "++" typedIncExp) "int"
											     ("char") -> TypedStatementExp (PostfixUnary "++" typedIncExp) "int"
											     _ -> error "Kann nicht inkrementiert werden"
										    where typedIncExp = addExp map incExp
addPostfix map (PostfixUnary "--" decExp) = case (exptype typedDecExp) of
											     ("int") -> TypedStatementExp (PostfixUnary "--" typedDecExp) "int"
											     ("char") -> TypedStatementExp (PostfixUnary "--" typedDecExp) "int"
											     _ -> error "Kann nicht dekrementiert werden"
										    where typedDecExp = addExp map decExp										  
										  
										  
fromJust :: (Maybe a) -> a
fromJust (Just a) = a
										  
