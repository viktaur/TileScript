module TileScriptEval where
import TileScriptGrammar
import System.IO
import TileScriptFunctions

-- import Data.Set
import Data.List

-- Data structures as defined in TileScriptGrammar:
-- data Exp  = TsExprSeparator Exp Exp | TsIf Exp Exp Exp | TsFor String Exp Exp | TsAssign Exp Exp
--           | TsIndexCell Exp Exp Exp | TsAdd Exp Exp | TsMul Exp Exp | TsSubtract Exp Exp | TsDivide Exp Exp 
--           | TsMod Exp Exp | TsGT Exp Exp | TsLT Exp Exp | TsGEqT Exp Exp | TsLEqT Exp Exp | TsVarDecl Type String Exp
--           | TsEquality Exp Exp | TsNotEquality Exp Exp | TsAndOperator Exp Exp | TsOrOperator Exp Exp
--           | TsPreDefFuncNewTile Exp | TsPreDefFuncNewBase Exp Exp Exp Exp | TsPreDefFuncReadTile Int | TsPreDefFuncExport Exp
--           | TsPreDefFuncNeg Exp | TsPreDefFuncXOR Exp Exp | TsPreDefFuncAND Exp Exp | TsPreDefFuncOR Exp Exp 
--           | TsPreDefFuncRotate Exp Exp | TsPreDefFuncCompose Exp Exp Exp Exp | TsPreDefFuncRange Exp | TsPreDefFuncSize Exp 
--           | TsPreDefFuncPlaceTile Exp Exp Exp Exp Exp | TsPreDefFuncPlaceBase Exp Exp Exp Exp Exp Exp | TsPreDefFuncScale Exp Exp
--           | TsPreDefFuncReflectY Exp | TsPreDefFuncReflectX Exp | TsPreDefFuncHeight Exp | TsPreDefFuncWidth Exp | TsPreDefFuncSubtile Exp Exp Exp
--           | TsInt Int | TsTrue | TsFalse | TsCharValue Char | TsStringValue String | TsVar String | TsPair Exp Exp | TsCell Int
--           | TsTile (TyArray (TyArray TyCell)) | TsBase (TyArray (TyArray TyCell)) | TsArrayDef ArrayContents | TsUnit     
--           deriving (Show, Eq)

-- data ArrayContents = ArrayContentsBase Exp | ArrayContentsRec Exp ArrayContents | ArrayContentsEmpty deriving (Show,Eq)

-- data Type = TyTile | TyCell | TyInt | TyString | TyChar | TyBool | TyUnit | TyBase | TyArray Type deriving (Show,Eq)

--  LIf Exp Exp Environment 
--             | LFor Exp Environment
--             | LAssign Exp Environment | RAssign Exp 

data Frame = LAdd Exp Environment | AddR Exp 
            | LMul Exp Environment | MulR Exp 
            | LSub Exp Environment | SubR Exp
            | LDiv Exp Environment | DivR Exp
            | LMod Exp Environment | ModR Exp 
            | LGT Exp Environment | GTR Exp 
            | LLT Exp Environment | LTR Exp
            | LGEQ Exp Environment | GEQR Exp 
            | LLEQ Exp Environment | LEQR Exp 
            | LEq Exp Environment | EqR Exp
            | LNEq Exp Environment | NEqR Exp 
            | LAnd Exp Environment | AndR Exp
            | LOr Exp Environment | OrR Exp 
            | LIf Exp Exp Environment 
            | LFor String Int Exp Environment | ForR String Int Int Exp Environment | ForRec String Int Int Exp Environment
            | RangeK 
            | FirstIndexCell Exp Exp Environment | SecondIndexCell [[Int]] Exp Environment | ThirdIndexCell [[Int]] Int
            | LExprSeparator Exp Environment
            | ExprSeparatorR Exp Environment
            | LRotate Exp Environment | RotateR Exp
            | NewTileK Exp | SizeK Exp
            | FirstNewBase Exp Exp Exp Exp Environment | SecondNewBase Int Exp Exp Exp Environment | ThirdNewBase Int Int Exp Exp Environment | FourthNewBase Int Int Int Exp Environment | FifthNewBase Int Int Int Int
            | ArrayContents [Exp] 
            | NegateK Exp | XorK Exp Environment | Xor [[Int]] | AndK Exp Environment | And [[Int]] | OrK Exp Environment | Or [[Int]] 
            | LCompose Exp Exp Exp Environment | LMCompose [[Int]] Exp Exp Environment | ComposeRM [[Int]] [[Int]] Exp Environment | ComposeR [[Int]] [[Int]] [[Int]]
            | FirstPlaceBase Exp Exp Exp Exp Exp Environment | SecondPlaceBase [[Int]] Exp Exp Exp Exp Environment | ThirdPlaceBase [[Int]] [[Int]] Exp Exp Exp Environment | FourthPlaceBase [[Int]] [[Int]] Int Exp Exp Environment | FifthPlaceBase [[Int]] [[Int]] Int Int Exp Environment | LastPlaceBase [[Int]] [[Int]] Int Int Int
            | LScale Exp Environment | ScaleR Exp 
            | ReflectYK Exp | ReflectXK Exp
            | HeightK Exp | WidthK Exp
            | FirstSubtile Exp Exp Environment | SecondSubtile [[Int]] Exp Environment | ThirdSubtile [[Int]] (Int, Int)
            | JoinVK Exp Environment | JoinV [[Int]] 
            | JoinHK Exp Environment | JoinH [[Int]]
            | ExportK Exp
            | FirstPlaceTile  Exp Exp Exp Exp Environment | SecondMPlaceTile [[Int]] Exp Exp Exp Environment | ThirdPlaceTile [[Int]] [[Int]] Exp Exp Environment | FourthPlaceTile [[Int]] [[Int]] Int Exp Environment | FifthPlaceTile [[Int]] [[Int]] Int Int 
            | VarDecK Type String | NormalAssign String | FirstCoolAssign String Exp Exp Environment | SecondCoolAssign String Int Exp Environment | ThirdCoolAssign String Int Int
            | RepeatXK Exp Environment | FirstRepeatX [[Int]] | RepeatYK Exp Environment | FirstRepeatY [[Int]] | ToBaseK
            deriving (Show, Eq)
type Kontinuation = [Frame]
type State = (Exp, Environment, Kontinuation)

-- An Environment maps a String that represents a variable with an Exp
-- (do not confuse with TypeEnvironment)

-- Define function unpack once we've figured out closures
-- unpack :: Exp -> (Exp, Environment)
-- unpack _ = undefined

-- Look up a value in an environment and unpack it
getValueFromEnv :: String -> Environment -> (Exp, Environment)
getValueFromEnv x [] = error ("Variable binding not found: " ++ x)
getValueFromEnv x ((t,y,e):env) | x == y  = (e,env) 
                                | otherwise = getValueFromEnv x env

-- Adds a new binding to an environment
-- update :: Environment -> Type -> String -> Exp -> Environment
-- update env x e = (x,e) : env

-- Checks for terminated expressions (TODO - Include all terminated Exp)
isValue :: Exp -> Bool
isValue (TsInt _ ) = True
isValue (TsTrue) = True
isValue (TsFalse) = True
isValue (TsUnit) = True
isValue (TsVar _ ) = True
isValue (TsCell _) = True
isValue (TsTile _) = True
isValue (TsBase _) = True
isValue (TsString _) = True
isValue (TsChar _) = True
isValue (TsArrayDef _ ) = True
isValue _ = False

-- Small step evaluation function (this is the big monster)
eval1 :: State -> State

eval1 ((TsVar x),env,k) = (e',env,k) 
                    where (e',env') = getValueFromEnv x env

-- = - = - = - = - = - = - = - = TERMINATED VALUES = - = - = - = - = - = - = - = --

eval1 (v,env,[]) | isValue v = (v,env,[])

-- = - = - = - = - = - = - = - = FOR OPERATIONS = - = - = - = - = - = - = - = --

eval1 ((TsInt c), env1, (LFor s i e env2):k) = (e, env2, (ForR s i c e env2):k)
eval1 (e1, env1, (ForR s 0 c e2 env2):k) | 0 == c = (TsUnit, updateWholeEnv env1 env2, k)
                                         | otherwise = (e2, placeInEnv (TyInt,s,TsInt 0) env1, (ForRec s 0 c e2 (placeInEnv (TyInt,s,TsInt 0) env2)):k)
-- eval1 ((TsUnit), env1, (ForRec s i c e2 env2):k) | i == c = (TsUnit, updateWholeEnv env1 env2, k)
eval1 ((TsUnit), env1, (ForRec s i c e2 env2):k) | i == (c - 1) = (TsUnit, env1, k)
                                                 | otherwise = (e2, (updateEnv TyInt s (TsInt (i+1)) env1), (ForRec s (i + 1) c e2 env2):k)
eval1 ((TsFor s c e), env, k) = (c, env, (LFor s 0 e env):k)

eval1 ((TsPreDefFuncRange e), env, k) = (e,env,(RangeK) : k)
eval1 (TsInt n,env,(RangeK):k) = (TsInt n,env,k)

-- = - = - = - = - = - = - = - = INT OPERATIONS = - = - = - = - = - = - = - = --

-- Add Operation 
eval1 ((TsAdd e1 e2),env,k) = (e1,env,(LAdd e2 env):k)
eval1 ((TsInt n),env1,(LAdd e env2):k) = (e,env2,(AddR (TsInt n)) : k)
eval1 ((TsInt m),env,(AddR (TsInt n)):k) = (TsInt (n + m),env,k)

-- Multiply Operation
eval1 ((TsMul e1 e2),env,k) = (e1,env,(LMul e2 env):k)
eval1 ((TsInt n),env1,(LMul e env2):k) = (e,env2,(MulR (TsInt n)) : k)
eval1 ((TsInt m),env,(MulR (TsInt n)):k) = (TsInt (n * m),env,k)

-- Subtract Operation
eval1 ((TsSubtract e1 e2),env,k) = (e1,env,(LSub e2 env):k)
eval1 ((TsInt n),env1,(LSub e env2):k) = (e,env2,(SubR (TsInt n)) : k)
eval1 ((TsInt m),env,(SubR (TsInt n)):k) = (TsInt (n - m),env,k)

-- Divide Operation
eval1 ((TsDivide e1 e2),env,k) = (e1,env,(LDiv e2 env):k)
eval1 ((TsInt n),env1,(LDiv e env2):k) = (e,env2,(DivR (TsInt n)) : k)
eval1 ((TsInt m),env,(DivR (TsInt n)):k) = (TsInt (n `div` m),env,k)

-- Modulo Operation
eval1 ((TsMod e1 e2),env,k) = (e1,env,(LMod e2 env):k)
eval1 ((TsInt n),env1,(LMod e env2):k) = (e,env2,(ModR (TsInt n)) : k)
eval1 ((TsInt m),env,(ModR (TsInt n)):k) = (TsInt (n `mod` m),env,k)

-- = - = - = - = - = - = - = - = BOOL OPERATIONS = - = - = - = - = - = - = - = --

-- Greater Than Operation 
eval1 ((TsGT e1 e2),env,k) = (e1,env,(LGT e2 env):k)
eval1 ((TsInt n),env1,(LGT e env2):k) = (e,env2,(GTR (TsInt n)) : k)
eval1 ((TsInt m),env,(GTR (TsInt n)):k) | n > m = (TsTrue,env,k)
                                        | otherwise = (TsFalse,env,k)

-- Less Than Operation 
eval1 ((TsLT e1 e2),env,k) = (e1,env,(LLT e2 env):k)
eval1 ((TsInt n),env1,(LLT e env2):k) = (e,env2,(LTR (TsInt n)) : k)
eval1 ((TsInt m),env,(LTR (TsInt n)):k) | n < m = (TsTrue,env,k)
                                        | otherwise = (TsFalse,env,k)

-- Greater Than/Equal To Operation 
eval1 ((TsGEqT e1 e2),env,k) = (e1,env,(LGEQ e2 env):k)
eval1 ((TsInt n),env1,(LGEQ e env2):k) = (e,env2,(GEQR (TsInt n)) : k)
eval1 ((TsInt m),env,(GEQR (TsInt n)):k) | n >= m = (TsTrue,env,k)
                                        | otherwise = (TsFalse,env,k)

-- Less Than/Equal To Operation
eval1 ((TsLEqT e1 e2),env,k) = (e1,env,(LLEQ e2 env):k)
eval1 ((TsInt n),env1,(LLEQ e env2):k) = (e,env2,(LEQR (TsInt n)) : k)
eval1 ((TsInt m),env,(LEQR (TsInt n)):k) | n <= m = (TsTrue,env,k)
                                        | otherwise = (TsFalse,env,k)

-- Equal To Operation
eval1 ((TsEquality e1 e2),env,k) = (e1,env,(LEq e2 env):k)
eval1 ((TsInt n),env1,(LEq e env2):k) = (e,env2,(EqR (TsInt n)) : k)
eval1 ((TsInt m),env,(EqR (TsInt n)):k) | n == m = (TsTrue,env,k)
                                        | otherwise = (TsFalse,env,k)

-- Not Equal To Operation
eval1 ((TsNotEquality e1 e2),env,k) = (e1,env,(LNEq e2 env):k)
eval1 ((TsInt n),env1,(LNEq e env2):k) = (e,env2,(NEqR (TsInt n)) : k)
eval1 ((TsInt m),env,(NEqR (TsInt n)):k) | n /= m = (TsTrue,env,k)
                                         | otherwise = (TsFalse,env,k)

-- AND Operation
eval1 ((TsAndOperator e1 e2),env,k) = (e1,env,(LAnd e2 env):k)
eval1 ((TsTrue),env1,(LAnd e env2):k) = (e,env2,(AndR (TsTrue)) : k)
eval1 ((TsFalse),env1,(LAnd e env2):k) = (e,env2,(AndR (TsFalse)) : k)
eval1 ((bool),env,(AndR (TsTrue)):k) = (bool, env, k)
eval1 ((bool),env,(AndR (TsFalse)):k) = (TsFalse, env, k)

-- OR Operation
eval1 ((TsOrOperator e1 e2),env,k) = (e1,env,(LOr e2 env):k)
eval1 ((TsTrue),env1,(LOr e env2):k) = (e,env2,(OrR (TsTrue)) : k)
eval1 ((TsFalse),env1,(LOr e env2):k) = (e,env2,(OrR (TsFalse)) : k)
eval1 ((bool),env,(OrR (TsTrue)):k) = (TsTrue, env, k)
eval1 ((bool),env,(OrR (TsFalse)):k) = (bool, env, k)

-- = - = - = - = - = - = - = - = IF-ELSE = - = - = - = - = - = - = - = --

eval1 ((TsIf e1 e2 e3),env,k) = (e1,env,(LIf e2 e3 env):k)
eval1 (TsTrue,env1,(LIf e2 e3 env2):k) = (e2,env2,k)
eval1 (TsFalse,env1,(LIf e2 e3 env2):k) = (e3,env2,k)

-- = - = - = - = - = - = - = - = INDEX CELL OPERATIONS = - = - = - = - = - = - = - = --


eval1 ((TsIndexCell v e1 e2),env,k) = (v,env,(FirstIndexCell e1 e2 env):k)
eval1 (TsTile ls,env1,(FirstIndexCell e1 e2 env2):k) = (e1,env2,(SecondIndexCell ls e2 env2):k)
eval1 (TsInt n1,env1,(SecondIndexCell ls e2 env2):k) = (e2,env2,(ThirdIndexCell ls n1):k)
eval1 (TsInt n2,env,(ThirdIndexCell ls n1):k) = (indexCell ls n1 n2,env,k)

-- = - = - = - = - = - = - = - = EXPRESION SEPARATOR OPERATIONS = - = - = - = - = - = - = - = --

eval1 ((TsExprSeparator e1 e2), env, k) = (e1, env, (LExprSeparator e2 env) : k)
eval1 ((TsUnit), env1, (LExprSeparator e2 env2) : k) = (e2, env1, (ExprSeparatorR (TsUnit) env1) : k)
eval1 (e@(TsString ls), env1 ,(ExprSeparatorR (TsUnit) env2) : k) = (e,[],[])
eval1 ((TsUnit), env2, (ExprSeparatorR (TsUnit) env1) : k) = ((TsUnit), (merge env2 env1), k)

-- = - = - = - = - = - = - = - = PREDEFINED FUNCTIONS = - = - = - = - = - = - = - = --

-- Newtile function
eval1 ((TsPreDefFuncNewTile e), env, k) = (e, env, (NewTileK e) : k)
eval1 ((TsInt n), env, (NewTileK e) : k) = ((newTile n), env, k)

-- Newbase function
eval1 ((TsPreDefFuncNewBase e1 e2 e3 e4 e5),env,k) = (e1,env,(FirstNewBase e2 e3 e4 e5 env):k)
eval1 ((TsInt n),env1,(FirstNewBase e2 e3 e4 e5 env2):k) = (e2,env2,(SecondNewBase n e3 e4 e5 env2):k)
eval1 ((TsInt n2),env1,(SecondNewBase n1 e3 e4 e5 env2):k) = (e3,env2,(ThirdNewBase n1 n2 e4 e5 env2):k)
eval1 ((TsInt n3),env1,(ThirdNewBase n1 n2 e4 e5 env2):k) = (e4,env2,(FourthNewBase n1 n2 n3 e5 env2):k)
eval1 ((TsInt n4),env1,(FourthNewBase n1 n2 n3 e5 env2):k) = (e5,env2,(FifthNewBase n1 n2 n3 n4):k)
eval1 ((TsArrayDef ls),env,(FifthNewBase n1 n2 n3 n4):k) = ((newBase n1 n2 n3 n4 (baseArrayContentsToLists ls env)), env, k)

-- ReadTile function
eval1 ((TsPreDefFuncReadTile e), env, k) = (fst tileVal,env,k)
    where
        tileVal = getValueFromEnv ("$" ++ (show e)) env

-- Rotate function
eval1 ((TsPreDefFuncRotate e1 e2), env, k) = (e1, env, (LRotate e2 env) : k)
eval1 ((TsTile ls), env, (LRotate e2 env') : k) = (e2, env', (RotateR (TsTile ls)) : k)
eval1 ((TsBase ls), env, (LRotate e2 env') : k) = (e2, env', (RotateR (TsTile ls)) : k)
eval1 ((TsInt n), env, (RotateR (TsTile ls)) : k) = ((rotate ls n), env, k)

-- Size function
eval1 ((TsPreDefFuncSize e), env, k) = (e, env, (SizeK e) : k)
eval1 ((TsTile ls), env, (SizeK e) : k) = (TsInt (length ls), env, k)

-- Export function
eval1((TsPreDefFuncExport e), env, k) = (e, env, (ExportK e) : k)
eval1((TsTile ls), env, (ExportK e) : k) = (export ls,env,k)
eval1((TsBase ls), env, (ExportK e) : k) = (export ls,env,k)

-- neg Function
eval1 ((TsPreDefFuncNeg e),env,k) = (e,env, (NegateK e):k)
eval1 ((TsTile ls),env,(NegateK e):k) = ((negF ls),env,k)

-- xor function
eval1 ((TsPreDefFuncXOR e1 e2),env,k) = (e1,env, (XorK e2 env):k)
eval1 ((TsTile ls1),env1,(XorK e2 env2):k) = (e2,env2,(Xor ls1):k)
eval1 ((TsTile ls2),env,(Xor ls1):k) = ((xorF ls1 ls2),env,k)

-- and function
eval1 ((TsPreDefFuncAND e1 e2),env,k) = (e1,env, (AndK e2 env):k)
eval1 ((TsTile ls1),env1,(AndK e2 env2):k) = (e2,env2,(And ls1):k)
eval1 ((TsTile ls2),env,(And ls1):k) = ((andF ls1 ls2),env,k)

-- or function
eval1 ((TsPreDefFuncOR e1 e2),env,k) = (e1,env, (OrK e2 env):k)
eval1 ((TsTile ls1),env1,(OrK e2 env2):k) = (e2,env2,(Or ls1):k)
eval1 ((TsTile ls2),env,(Or ls1):k) = ((orF ls1 ls2),env,k)

-- compose function
eval1 ((TsPreDefFuncCompose e1 e2 e3 e4),env,k) = (e1,env,(LCompose e2 e3 e4 env):k)
eval1 (TsTile ls,env1,(LCompose e2 e3 e4 env2):k) = (e2,env2,(LMCompose ls e3 e4 env2):k)
eval1 (TsTile ls2,env1,(LMCompose ls1 e3 e4 env2):k) = (e3,env2,(ComposeRM ls1 ls2 e4 env2):k)
eval1 (TsTile ls3,env1,(ComposeRM ls1 ls2 e4 env2):k) = (e4,env2,(ComposeR ls1 ls2 ls3):k)
eval1 (TsTile ls4,env1,(ComposeR ls1 ls2 ls3):k) = (compose ls1 ls2 ls3 ls4,env1,k)

-- placeBase function
eval1 ((TsPreDefFuncPlaceBase e1 e2 e3 e4 e5 e6),env,k) = (e1,env,(FirstPlaceBase e2 e3 e4 e5 e6 env):k)
eval1 (TsBase ls,env1,(FirstPlaceBase e2 e3 e4 e5 e6 env2):k) = (e2,env2,(SecondPlaceBase ls e3 e4 e5 e6 env2):k)
eval1 (TsTile ls2,env1,(SecondPlaceBase ls1 e3 e4 e5 e6 env2):k) = (e3,env2,(ThirdPlaceBase ls1 ls2 e4 e5 e6 env2):k)
eval1 (TsInt n1,env1,(ThirdPlaceBase ls1 ls2 e4 e5 e6 env2):k) = (e4,env2,(FourthPlaceBase ls1 ls2 n1 e5 e6 env2):k)
eval1 (TsInt n2,env1,(FourthPlaceBase ls1 ls2 n1 e5 e6 env2):k) = (e5,env2,(FifthPlaceBase ls1 ls2 n1 n2 e6 env2):k)
eval1 (TsInt n3,env1,(FifthPlaceBase ls1 ls2 n1 n2 e6 env2):k) = (e6,env2,(LastPlaceBase ls1 ls2 n1 n2 n3):k)
eval1 (TsInt n4,env,(LastPlaceBase ls1 ls2 n1 n2 n3):k) = (placeBase ls1 ls2 n1 n2 n3 n4, env, k)

-- scale function
eval1 (TsPreDefFuncScale e1 e2,env,k) = (e1,env,(LScale e2 env):k)
eval1 ((TsTile ls),env1,(LScale e2 env2):k) = (e2,env2,(ScaleR (TsTile ls)):k)
eval1 ((TsInt n),env1,(ScaleR (TsTile ls)):k) = ((scaleTile ls n), env1, k)
eval1 ((TsInt n),env1,(ScaleR (TsBase ls)):k) = ((scaleBase ls n), env1, k)

-- reflectY function
eval1 (TsPreDefFuncReflectY e,env,k) = (e,env,(ReflectYK e):k)
eval1 (TsTile ls,env,(ReflectYK e):k) = ((reflectYTile ls),env,k)
eval1 (TsBase ls,env,(ReflectYK e):k) = ((reflectYBase ls),env,k)

-- reflectX function
eval1 (TsPreDefFuncReflectX e,env,k) = (e,env,(ReflectXK e):k)
eval1 (TsTile ls,env,(ReflectXK e):k) = ((reflectXTile ls),env,k)
eval1 (TsBase ls,env,(ReflectXK e):k) = ((reflectXBase ls),env,k)

-- height function
eval1 (TsPreDefFuncHeight e,env,k) = (e,env,(HeightK e):k)
-- eval1 (TsTile ls,env,(HeightK e):k) = (height ls,env,k)
eval1 (TsBase ls,env,(HeightK e):k) = (height ls,env,k)

-- width function
eval1 (TsPreDefFuncWidth e,env,k) = (e,env,(WidthK e):k)
-- eval1 (TsTile ls,env,(WidthK e):k) = (width ls,env,k)
eval1 (TsBase ls,env,(WidthK e):k) = (width ls,env,k)

-- subtile function
eval1 (TsPreDefFuncSubtile e1 e2 e3,env,k) = (e1,env,(FirstSubtile e2 e3 env):k)
eval1 (TsTile ls,env1,(FirstSubtile e2 e3 env2):k) = (e2,env2,(SecondSubtile ls e3 env2):k)
eval1 (TsPair (TsInt n1) (TsInt n2),env1,(SecondSubtile ls e3 env2):k) = (e3,env2,(ThirdSubtile ls (n1, n2)):k)
eval1 (TsInt n3,env,(ThirdSubtile ls (n1, n2)):k) = (subtile ls (n1,n2) n3,env,k)

-- joinVertically function
eval1 ((TsPreDefFuncJoinY e1 e2),env,k) = (e1,env, (JoinVK e2 env):k)
eval1 ((TsTile ls1),env1,(JoinVK e2 env2):k) = (e2,env2,(JoinV ls1):k)
eval1 ((TsBase ls1),env1,(JoinVK e2 env2):k) = (e2,env2,(JoinV ls1):k)
eval1 ((TsTile ls2),env,(JoinV ls1):k) = (joinVertically ls1 ls2,env,k)
eval1 ((TsBase ls2),env,(JoinV ls1):k) = (joinVertically ls1 ls2,env,k)

-- joinHorizontally function
eval1 ((TsPreDefFuncJoinX e1 e2),env,k) = (e1,env, (JoinHK e2 env):k)
eval1 ((TsTile ls1),env1,(JoinHK e2 env2):k) = (e2,env2,(JoinH ls1):k)
eval1 ((TsBase ls1),env1,(JoinHK e2 env2):k) = (e2,env2,(JoinH ls1):k)
eval1 ((TsTile ls2),env,(JoinH ls1):k) = (joinHorizontally ls1 ls2,env,k)
eval1 ((TsBase ls2),env,(JoinH ls1):k) = (joinHorizontally ls1 ls2,env,k)

-- Place Tile Function TsPreDefFuncPlaceTile
eval1 ((TsPreDefFuncPlaceTile e1 e2 e3 e4 e5), env, k) = (e1, env, (FirstPlaceTile e2 e3 e4 e5 env) : k)
eval1 (TsTile ls, env1, ((FirstPlaceTile e2 e3 e4 e5 env2) : k)) = (e2, env2, (SecondMPlaceTile ls e3 e4 e5 env2) : k)
eval1 (TsTile ls2, env1, (SecondMPlaceTile ls e3 e4 e5 env2) : k) = (e3, env2, (ThirdPlaceTile ls ls2 e4 e5 env2) : k)
eval1 (TsInt n, env1, (ThirdPlaceTile ls ls2 e4 e5 env2) : k) = (e4, env2, (FourthPlaceTile ls ls2 n e5 env2) : k)
eval1 (TsInt n2, env1, (FourthPlaceTile ls ls2 n1 e5 env2) : k) = (e5, env2, (FifthPlaceTile ls ls2 n1 n2) : k)
eval1 (TsInt n3, env, (FifthPlaceTile ls ls2 n1 n2) : k) = (placeTile ls ls2 n1 n2 n3, env, k)

--RepeatX Tile / Base Function
eval1 ((TsPreDefFuncRepeatX e1 e2), env, k) = (e1, env, (RepeatXK e2 env) : k)
eval1 ((TsTile ls, env1, ((RepeatXK e2 env2) : k))) = (e2, env2, (FirstRepeatX ls) : k)
eval1 ((TsBase ls, env1, ((RepeatXK e2 env2) : k))) = (e2, env2, (FirstRepeatX ls) : k)
eval1 ((TsInt n, env, ((FirstRepeatX ls) : k))) = (repeatX ls n, env, k)

--RepeatY Tile / Base Function
eval1 ((TsPreDefFuncRepeatY e1 e2), env, k) = (e1, env, (RepeatYK e2 env) : k)
eval1 ((TsTile ls, env1, ((RepeatYK e2 env2) : k))) = (e2, env2, (FirstRepeatY ls) : k)
eval1 ((TsBase ls, env1, ((RepeatYK e2 env2) : k))) = (e2, env2, (FirstRepeatY ls) : k)
eval1 ((TsInt n, env, ((FirstRepeatY ls) : k))) = (repeatY ls n, env, k)

--ToBase function
eval1 ((TsPreDefFuncToBase e),env,k) = (e,env,(ToBaseK):k)
eval1 ((TsTile ls),env,(ToBaseK):k) = ((toBase ls),env,k)
-- = - = - = - = - = - = - = - = ARRAY DEFINITION = - = - = - = - = - = - = - = --

-- eval1 ((TsArrayDef e),env,k) = (e,)
-- eval1 ((TsArrayDef e),env,k) = (e,)

-- = - = - = - = - = - = - = - = VAR DECLARATION & ASSIGN OPERATIONS = - = - = - = - = - = - = - = --

eval1 ((TsVarDecl t v (TsArrayDef e)),env,k) | t == TyTile = (TsUnit,placeInEnv (t,v,TsTile $ arrayContentsToLists e env) env,k)
                                             | t == TyBase = (TsUnit,placeInEnv (t,v,TsBase $ arrayContentsToLists e env) env,k)
                                             | otherwise = error "cannot declare a variable that is not a base or tile with array contents"

eval1 ((TsVarDecl t v e),env,k) = (e,env,(VarDecK t v):k)
eval1 (e,env,(VarDecK t v):k) | isValue e = (TsUnit,placeInEnv (t,v,e) env,k)
                              | otherwise = error "The expression for variable declaration does not evaluate to a value"

eval1 (TsAssign (TsIndexCell (TsVar v) i j) e,env,k) = (i,env,(FirstCoolAssign v j e env):k)
eval1 (TsInt i,evn1,(FirstCoolAssign v j e env2):k) = (j,env2,(SecondCoolAssign v i e env2):k)
eval1 (TsInt j,evn1,(SecondCoolAssign v i e env2):k) = (e,env2,(ThirdCoolAssign v i j):k)
eval1 (TsCell x,env,(ThirdCoolAssign v i j):k) = if getTypeOfVar v env == TyTile then return1 else return2
    where
        return1 = (TsUnit,updateEnv TyTile v (tileCellIndexReassing (getPureTileOrBase $ fst $ getValueFromEnv v env) x i j) env,k)
        return2 = (TsUnit,updateEnv TyBase v (baseCellIndexReassing (getPureTileOrBase $ fst $ getValueFromEnv v env) x i j) env,k)
eval1 (TsInt x,env,(ThirdCoolAssign v i j):k) = if getTypeOfVar v env == TyTile then return1 else return2
    where
        return1 = (TsUnit,updateEnv TyTile v (tileCellIndexReassing (getPureTileOrBase $ fst $ getValueFromEnv v env) x i j) env,k)
        return2 = (TsUnit,updateEnv TyBase v (baseCellIndexReassing (getPureTileOrBase $ fst $ getValueFromEnv v env) x i j) env,k)

eval1 (TsAssign (TsVar x) e,env,k) = (e,env,(NormalAssign x):k)
eval1 (e@(TsInt n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyInt x e env,k)
eval1 (e@(TsTrue),env,(NormalAssign x):k) = (TsUnit,updateEnv TyBool x e env,k)
eval1 (e@(TsFalse),env,(NormalAssign x):k) = (TsUnit,updateEnv TyBool x e env,k)
eval1 (e@(TsCell n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyCell x e env,k)
eval1 (e@(TsTile n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyTile x e env,k)
eval1 (e@(TsBase n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyBase x e env,k)
eval1 (e@(TsString n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyString x e env,k)
eval1 (e@(TsChar n),env,(NormalAssign x):k) = (TsUnit,updateEnv TyChar x e env,k)

eval1 (e,_,k) = error ("\nerror could not find any pattern match for " ++ "\n" ++ (show e) ++ "\n" ++ (show $ head k))

-- Function to iterate the small step reduction to termination
evalLoop :: Exp -> Environment -> Exp 
evalLoop e env = evalLoop' (e,env,[])

evalLoop' :: State -> Exp
evalLoop' (e,env,k) = if (e' == e) && (isValue e') && (null k) then e' else evalLoop' (e',env',k')
    where (e',env',k') = eval1 (e,env,k)

-- Helper functions
merge :: Environment -> Environment -> Environment
-- merge xs ys = [(t,v,e) | (t,v,e) <- xs, (t',v',e') <- ys , v == v'] ++ [(t',v',e') | (t,v,e) <- xs, (t',v',e') <- ys , v /= v'] 
merge xs ys = nub $ xs ++ (filter (\y -> not $ y `elem` xs) ys)

updateWholeEnv :: Environment -> Environment -> Environment
updateWholeEnv xs ys = [(t,v,e') | (t,v,e) <- xs, (t',v',e') <- ys , v == v']

updateEnv :: Type -> String -> Exp -> Environment -> Environment
updateEnv newT v e ((t,x,e'):xs) | v == x = (newT,x,e) : xs
                                 | otherwise = (t,x,e') : updateEnv newT v e xs

tileCellIndexReassing :: [[Int]] -> Int -> Int -> Int -> Exp
tileCellIndexReassing xss newVal i j = TsTile (take i xss ++ [newLs] ++ drop (i+1) xss)
    where
        newLs = take j (xss !! i) ++ [newVal] ++ drop (j+1) (xss !! i)

baseCellIndexReassing :: [[Int]] -> Int -> Int -> Int -> Exp
baseCellIndexReassing xss newVal i j = TsBase (take i xss ++ [newLs] ++ drop (i+1) xss)
    where
        newLs = take j (xss !! i) ++ [newVal] ++ drop (j+1) (xss !! i)

getTypeOfVar :: String -> Environment -> Type
getTypeOfVar s [] = error ("Could not find variable in environment: " ++ s)
getTypeOfVar s ((t,v,e):xs) | s == v = t
                            | otherwise = getTypeOfVar s xs

getPureInt :: Exp -> Int
getPureInt (TsInt n) = n

getPureTileOrBase :: Exp -> [[Int]]
getPureTileOrBase (TsTile ls) = ls
getPureTileOrBase (TsBase ls) = ls


debug :: State -> Int -> IO ()
debug e n = do 
    let x = take n $ iterate eval1 e
    writeFile "debugFile.txt" (debug' x)
        where
            debug' [] = []
            debug' (x:xs) = show x ++ "\n\n" ++ debug' xs

placeInEnv :: (Type, String, Exp) -> Environment -> Environment
placeInEnv (t,v,e) [] = [(t,v,e)]
placeInEnv (t,v,e) ((t',v',e'):xs) | v == v' = (t,v,e) : xs
                                   | otherwise = (t',v',e') : placeInEnv (t,v,e) xs

-- = - = - = - = - = - = - = - = BASE ARRAYS  = - = - = - = - = - = - = - = --

baseArrayContentsToLists :: ArrayContents -> Environment -> [[[[Int]]]]
baseArrayContentsToLists ArrayContentsEmpty env = []
baseArrayContentsToLists (ArrayContentsBase exp) env = [baseExpToList exp env]
baseArrayContentsToLists (ArrayContentsRec exp rest) env = (baseExpToList exp env) : baseArrayContentsToLists rest env

arrayContentsToLists :: ArrayContents -> Environment -> [[Int]]
arrayContentsToLists ArrayContentsEmpty env = []
arrayContentsToLists (ArrayContentsBase exp) env = [expToList exp env]
arrayContentsToLists (ArrayContentsRec exp rest) env = (expToList exp env) : arrayContentsToLists rest env

expToList :: Exp -> Environment -> [Int]
expToList (TsInt n) env = [n]
expToList (TsVar n) env = expToList (fst (getValueFromEnv n env)) env
expToList (TsArrayDef contents) env = concat (arrayContentsToLists contents env)

baseExpToList :: Exp -> Environment -> [[[Int]]]
baseExpToList (TsTile n) env = [n]
baseExpToList (TsBase n) env = [n]
baseExpToList (TsVar n) env = baseExpToList (fst (getValueFromEnv n env)) env
baseExpToList (TsArrayDef contents) env = concat (baseArrayContentsToLists contents env)

-- Function to unparse underlying values from the AST term (TODO - Include all terminated Exp)
unparse :: Exp -> String 
unparse (TsInt n) = show n
unparse (TsTrue) = "true"
unparse (TsFalse) = "false"
unparse (TsUnit) = "()"
unparse (TsPair e1 e2) = "( " ++ (unparse e1) ++ " , " ++ (unparse e2) ++ " )"
unparse (TsCell n) = show n
unparse (TsTile ls) = show ls
unparse (TsBase ls) = show ls
unparse (TsString s) = s
unparse (TsChar c) = c : []
-- ...
unparse _ = "Unknown"