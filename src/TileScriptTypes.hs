module TileScriptTypes where
import TileScriptGrammar

-- Data structures as defined in TileScriptGrammar:
-- data Exp  = TsExprSeparator Exp Exp | TsIf Exp Exp Exp | TsFor String Exp Exp | TsAssign Exp Exp
--           | TsIndexCell Exp Exp Exp | TsAdd Exp Exp | TsMul Exp Exp | TsSubtract Exp Exp | TsDivide Exp Exp 
--           | TsMod Exp Exp | TsGT Exp Exp | TsLT Exp Exp | TsGEqT Exp Exp | TsLEqT Exp Exp | TsVarDecl Type String Exp
--           | TsEquality Exp Exp | TsNotEquality Exp Exp | TsAndOperator Exp Exp | TsOrOperator Exp Exp
--           | TsPreDefFuncNewTile Exp | TsPreDefFuncNewBase Exp Exp Exp Exp Exp | TsPreDefFuncReadTile Int | TsPreDefFuncExport Exp
--           | TsPreDefFuncNeg Exp | TsPreDefFuncXOR Exp Exp | TsPreDefFuncAND Exp Exp | TsPreDefFuncOR Exp Exp 
--           | TsPreDefFuncRotate Exp Exp | TsPreDefFuncCompose Exp Exp Exp Exp | TsPreDefFuncRange Exp | TsPreDefFuncSize Exp 
--           | TsPreDefFuncPlaceTile Exp Exp Exp Exp Exp | TsPreDefFuncPlaceBase Exp Exp Exp Exp Exp Exp | TsPreDefFuncScale Exp Exp
--           | TsPreDefFuncReflectY Exp | TsPreDefFuncReflectX Exp | TsPreDefFuncHeight Exp | TsPreDefFuncWidth Exp | TsPreDefFuncSubtile Exp Exp Exp 
--           | TsPreDefFuncJoinY Exp Exp | TsPreDefFuncJoinX Exp Exp | TsPreDefFuncRepeatX Exp Exp | TsPreDefFuncRepeatY Exp Exp 
--           | TsInt Int | TsTrue | TsFalse | TsChar Char | TsString String | TsVar String | TsPair Exp Exp | TsCell Int
--           | TsTile [[Int]] | TsBase [[Int]] | TsArrayDef ArrayContents | TsUnit     
--           deriving (Show, Eq)

-- data ArrayContents = ArrayContentsBase Exp | ArrayContentsRec Exp ArrayContents | ArrayContentsEmpty deriving (Show,Eq)

-- data Type = TyTile | TyCell | TyInt | TyString | TyChar | TyBool | TyUnit | TyBase | TyPair | TyArray Type deriving (Show,Eq)


type TypeEnvironment = [(String, Type)]

getBinding :: String -> TypeEnvironment -> Type
getBinding x [] = error "Variable binding not found"
getBinding x ((y,t) : tenv) | x == y    = t
                            | otherwise = getBinding x tenv

addBinding :: String -> Type -> TypeEnvironment -> TypeEnvironment
addBinding x t tenv = (x,t):tenv

merge :: TypeEnvironment -> TypeEnvironment -> TypeEnvironment
merge xs ys = xs ++ (filter (\y -> not $ y `elem` xs) ys)


typeOf :: TypeEnvironment -> Exp -> (Type, TypeEnvironment)
-- typeOf tenv (TsExprSeparator e1 e2)
--     | (TyUnit, TyUnit) == (typeOf tenv e1, typeOf tenv e2) = TyUnit

typeOf tenv (TsExprSeparator e1 e2) = case typeOf tenv e1 of
    (TyUnit, tenv1) -> case typeOf tenv1 e2 of
        (TyUnit, tenv2) -> (TyUnit, merge tenv1 tenv2)
        otherwise -> error "Statements need to be of Unit type"
    otherwise -> error "Statements need to be of Unit type"


typeOf tenv (TsIf c e1 e2) | TyBool == (fst $ typeOf tenv c) && t1 == t2 = (t1, tenv)
    where (t1, t2) = (fst $ typeOf tenv e1, fst $ typeOf tenv e2)

-- TODO!(Figure this out, might need to change grammar)
typeOf tenv (TsFor x c e )  
    | (TyInt, TyUnit) == (fst $ typeOf tenv c, fst $ typeOf (addBinding x TyInt tenv) e) = (TyUnit, tenv)

typeOf tenv (TsAssign e1 e2) | t1 == t2 = (TyUnit, tenv)
    where (t1, t2) = (fst $ typeOf tenv e1, fst $ typeOf tenv e2)

typeOf tenv (TsIndexCell var i1 i2)
    | (TyTile, TyInt, TyInt) == (fst $ typeOf tenv var, fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)


-- arithmetic operators
typeOf tenv (TsAdd i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)

typeOf tenv (TsMul i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)

typeOf tenv (TsSubtract i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)

typeOf tenv (TsDivide i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)

typeOf tenv (TsMod i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyInt, tenv)


-- comparator operators
typeOf tenv (TsGT i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyBool, tenv)

typeOf tenv (TsLT i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyBool, tenv)

typeOf tenv (TsGEqT i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyBool, tenv)

typeOf tenv (TsLEqT i1 i2)
    | (TyInt, TyInt) == (fst $ typeOf tenv i1, fst $ typeOf tenv i2) = (TyBool, tenv)

typeOf tenv (TsVarDecl t s e ) | t == (fst $ typeOf tenv e) = (TyUnit, addBinding s t tenv)

typeOf tenv (TsEquality e1 e2) | t1 == t2 = (TyBool, tenv)
    where (t1, t2) = (fst $ typeOf tenv e1, fst $ typeOf tenv e2)

typeOf tenv (TsNotEquality e1 e2) | t1 == t2 = (TyBool, tenv)
    where (t1, t2) = (fst $ typeOf tenv e1, fst $ typeOf tenv e2)


-- boolean operators
typeOf tenv (TsAndOperator e1 e2)
    | (TyBool, TyBool) == (fst $ typeOf tenv e1, fst $ typeOf tenv e2) = (TyBool, tenv)

typeOf tenv (TsOrOperator e1 e2)
    | (TyBool, TyBool) == (fst $ typeOf tenv e1, fst $ typeOf tenv e2) = (TyBool, tenv)


-- TODO!(All predefined functions)

typeOf tenv (TsPreDefFuncNewTile i )
    | TyInt == (fst $ typeOf tenv i) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncNewBase n1 n2 i1 i2 array )
    | (TyInt, TyInt, TyInt, TyInt, TyArray (TyArray TyTile)) == (fst $ typeOf tenv n1, fst $ typeOf tenv n2, fst $ typeOf tenv i1, fst $ typeOf tenv i2, fst $ typeOf tenv array) = (TyBase, tenv)
    | (TyInt, TyInt, TyInt, TyInt, TyArray (TyArray TyUnit)) == (fst $ typeOf tenv n1, fst $ typeOf tenv n2, fst $ typeOf tenv i1, fst $ typeOf tenv i2, fst $ typeOf tenv array) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncReadTile n ) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncExport t )
    | (TyTile) == (fst $ typeOf tenv t) = (TyUnit, tenv)
    | (TyBase) == (fst $ typeOf tenv t) = (TyUnit, tenv)

typeOf tenv (TsPreDefFuncNeg t )
    | TyTile == (fst $ typeOf tenv t) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncXOR t1 t2 )
    | (TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncAND t1 t2 )
    | (TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncOR t1 t2 )
    | (TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncRotate t i )
    | (TyTile, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv i) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncCompose t1 t2 t3 t4 )
    | (TyTile, TyTile, TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2, fst $ typeOf tenv t3, fst $ typeOf tenv t4) = (TyTile, tenv)

-- typeOf tenv (TsPreDefFuncRange i )
--     | TyInt == (fst $ typeOf tenv i) = 

typeOf tenv (TsPreDefFuncSize t )
    | TyTile == (fst $ typeOf tenv t) = (TyInt, tenv)

typeOf tenv (TsPreDefFuncPlaceTile t o n i j )
    | (TyTile, TyTile, TyInt, TyInt, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv o, fst $ typeOf tenv n, fst $ typeOf tenv i, fst $ typeOf tenv j) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncPlaceBase b o h w i j )
    | (TyBase, TyTile, TyInt, TyInt, TyInt, TyInt) == (fst $ typeOf tenv b, fst $ typeOf tenv o, fst $ typeOf tenv h, fst $ typeOf tenv w, fst $ typeOf tenv i, fst $ typeOf tenv j) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncScale e i )
    | (TyTile, TyInt) == (fst $ typeOf tenv e, fst $ typeOf tenv i) = (TyTile, tenv)
    | (TyBase, TyInt) == (fst $ typeOf tenv e, fst $ typeOf tenv i) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncReflectX t )
    | TyTile == (fst $ typeOf tenv t) = (TyTile, tenv)
    | TyBase == (fst $ typeOf tenv t) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncReflectY t )
    | TyTile == (fst $ typeOf tenv t) = (TyTile, tenv)
    | TyBase == (fst $ typeOf tenv t) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncHeight b )
    | TyBase == (fst $ typeOf tenv b) = (TyInt, tenv)

typeOf tenv (TsPreDefFuncWidth b )
    | TyBase == (fst $ typeOf tenv b) = (TyInt, tenv)

typeOf tenv (TsPreDefFuncSubtile t p n )
    | (TyTile, TyPair, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv p, fst $ typeOf tenv n) = (TyTile, tenv)

typeOf tenv (TsPreDefFuncJoinY t1 t2)
    | (TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyTile, TyBase) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyBase, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyBase, TyBase) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncJoinX t1 t2)
    | (TyTile, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyTile, TyBase) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyBase, TyTile) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)
    | (TyBase, TyBase) == (fst $ typeOf tenv t1, fst $ typeOf tenv t2) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncRepeatX t n)
    | (TyTile, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv n) = (TyBase, tenv)
    | (TyBase, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv n) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncRepeatY t n)
    | (TyTile, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv n) = (TyBase, tenv)
    | (TyBase, TyInt) == (fst $ typeOf tenv t, fst $ typeOf tenv n) = (TyBase, tenv)

typeOf tenv (TsPreDefFuncToBase t )
    | (TyTile) == (fst $ typeOf tenv t) = (TyBase, tenv)

typeOf tenv (TsInt _ ) = (TyInt, tenv)

typeOf tenv (TsChar _ ) = (TyChar, tenv)

typeOf tenv (TsString _ ) = (TyString, tenv)

typeOf tenv (TsTrue) = (TyBool, tenv)

typeOf tenv (TsFalse) = (TyBool, tenv)

typeOf tenv (TsVar x) = (getBinding x tenv, tenv)

typeOf tenv (TsUnit) = (TyUnit, tenv)

typeOf tenv (TsCell _ ) = (TyCell, tenv)

typeOf tenv (TsTile _ ) = (TyTile, tenv)

typeOf tenv (TsBase _ ) = (TyBase, tenv)

typeOf tenv (TsArrayDef (ArrayContentsBase e )) = (TyArray (fst $ typeOf tenv e), tenv)

typeOf tenv (TsArrayDef (ArrayContentsRec e _ )) = (TyArray (fst $ typeOf tenv e), tenv)

typeOf tenv (TsArrayDef (ArrayContentsEmpty)) = (TyArray (TyUnit), tenv)

typeOf tenv (TsPair l r )
    | (TyInt, TyInt) == (fst $ typeOf tenv l, fst $ typeOf tenv r) = (TyPair, tenv)

typeOf tenv _ = error "Type error"