----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario
import Prelude
import Data.List
import Data.Maybe

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


checkProgram :: Program -> Checked
checkProgram program = repeatedName program program

--2.1 REPETICION DE NOMBRES--------------------------------------------------------------

repeatedName :: Program -> Program -> Checked
repeatedName program (Program defs _) = errorDuplicated program ((repeatedIn funNames) ++ (concat (map repeatedIn paramNames)))
  where funNames = toNames defs
        paramNames = toParameters defs

toNames :: [FunDef] -> [Name]
toNames [] = []
toNames ((FunDef (name, _) _ _):fs) = name : toNames fs

toParameters :: [FunDef] -> [[Name]]
toParameters [] = []
toParameters ((FunDef (_,_) names _):fs) = names : toParameters fs

errorDuplicated :: Program -> [Error] -> Checked
errorDuplicated program [] = numberOfParameters program program
errorDuplicated _ errors = Wrong errors

repeatedIn :: [Name] -> [Error]
repeatedIn names = repeatedAux names [] []

repeatedAux :: [Name] -> [Name] -> [Name] -> [Error]
repeatedAux [] _ repeated = (map getDuplicatedError repeated)
repeatedAux (n:ns) seen repeated
  = if n `elem` seen then repeatedAux ns seen (repeated ++ [n])
    else repeatedAux ns (n:seen) repeated

nextTo :: Name -> [Name] -> [Name]
nextTo name (n:ns)
  = if n == name then (name:(n:ns))
    else n : nextTo name ns

getDuplicatedError :: Name -> Error
getDuplicatedError name = Duplicated name

--2.2 NUMERO DE PARAMETROS-----------------------------------------------------

numberOfParameters :: Program -> Program -> Checked
numberOfParameters program (Program defs _) = errorArgNumDef program errors
  where errors = matchArgs defs

errorArgNumDef :: Program -> [Error] -> Checked
errorArgNumDef program [] = undeclaredNames program program
errorArgNumDef program errors = Wrong errors

matchArgs :: [FunDef] -> [Error]
matchArgs [] = []
matchArgs ((FunDef (name, Sig types _) names _):fs) 
  = if nlen == tlen then matchArgs fs
    else (ArgNumDef name tlen nlen) : matchArgs fs
  where nlen = length names
        tlen = length types

--2.3 NOMBRES NO DECLARADOS------------------------------------------------------

undeclaredNames :: Program -> Program -> Checked
undeclaredNames program (Program defs body) = errorUndefined program errors
  where errors = (matchArgNames funNames defs) ++ (matchParamsInExp body [] funNames)
        funNames = toNames defs

errorUndefined :: Program -> [Error] -> Checked
errorUndefined program [] = typeCheck program
errorUndefined program errors = Wrong errors

matchArgNames :: [Name] -> [FunDef] -> [Error]
matchArgNames _ [] = []
matchArgNames funNames ((FunDef (name, Sig types ty) names expr):fs) = (matchParamsInExp expr names funNames) ++ (matchArgNames funNames fs)

matchParamsInExp :: Expr -> [Name] -> [Name] -> [Error]
matchParamsInExp (Var name) args _
  = if name `elem` args then [] 
    else [Undefined name]
matchParamsInExp (Let (name, t) e1 e2) args funNames = (matchParamsInExp e1 args funNames) ++ (matchParamsInExp e2 (name:args) funNames)
matchParamsInExp (If e1 e2 e3) args funNames = (matchParamsInExp e1 args funNames) ++ (matchParamsInExp e2 args funNames) ++ (matchParamsInExp e3 args funNames)
matchParamsInExp (Infix _ e1 e2) args funNames = (matchParamsInExp e1 args funNames) ++ (matchParamsInExp e2 args funNames)
matchParamsInExp (App name es) args funNames 
  = if name `elem` funNames then matchParamsToAll es args funNames
    else (Undefined name) : (matchParamsToAll es args funNames)
matchParamsInExp _ _ _ = []

matchParamsToAll :: [Expr] -> [Name] -> [Name] -> [Error]
matchParamsToAll [] _ _ = []
matchParamsToAll (e:es) args funNames = (matchParamsInExp e args funNames) ++ (matchParamsToAll es args funNames)

--2.4 CHEQUEO DE TIPOS---------------------------------------------------------------

typeCheck :: Program -> Checked
typeCheck (Program defs body) = errorType errors 
  where envF = createEnvFun defs
        errorsDefs = checkFunctions defs envF defs
        errorsBody = getErrors [] envF body
        errors = errorsDefs ++ errorsBody

checkFunctions :: [FunDef] -> [TypedFun] -> [FunDef] -> [Error]
checkFunctions _ _ [] = []
checkFunctions defs envF ((FunDef (_, (Sig types t)) names expr) : fs) = (matchTypeExpr env envF t expr) ++ checkFunctions defs envF fs
  where env = zip names types

errorType :: [Error] -> Checked
errorType [] = Ok
errorType errors = Wrong errors

--Crea ambiente de variables-------------------
createEnv :: [Name] -> [Type] -> Env
createEnv names types = zip names types 

--Crea ambiente de funciones------------------
createEnvFun :: [FunDef] -> [TypedFun]
createEnvFun [] = []
createEnvFun ((FunDef tf _ _):fs) = tf : createEnvFun(fs)

---matchTypeExpr : matchea la expresion con su tipo esperado-----dado un ambiente de variables y otro de funciones
matchTypeExpr :: Env -> [TypedFun] -> Type -> Expr -> [Error]
matchTypeExpr env envF t (Var name) = matchVarType name t env
matchTypeExpr env envF t (BoolLit _) = matchTypes t TyBool
matchTypeExpr env envF t (IntLit _) = matchTypes t TyInt
matchTypeExpr env envF t (If e1 e2 e3) = (matchTypes t t2) ++ (matchTypes TyBool t1) ++ (matchTypes t2 t3) ++ (getErrors env envF e1) ++ (getErrors env envF e2) ++ (getErrors env envF e3)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
        t3 = getType env envF e3
matchTypeExpr env envF t (App name exps)
  = if lenExps == lenArgs then (matchTypes t fType) ++ (matchTypeExprS env envF fArgTypes exps)
    else (matchTypes t fType) ++ [(ArgNumApp name lenArgs lenExps)] ++ (matchTypeExprS env envF fArgTypes exps)
  where lenExps = length exps
        lenArgs = cantArgs name envF
        fType = funType name envF
        fArgTypes = funArgTypes name envF
matchTypeExpr env envF t (Infix Add e1 e2) = (matchTypes t TyInt) ++ (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
matchTypeExpr env envF t (Infix Sub e1 e2) = (matchTypes t TyInt) ++ (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
matchTypeExpr env envF t (Infix Mult e1 e2) = (matchTypes t TyInt) ++ (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
matchTypeExpr env envF t (Infix Div e1 e2) = (matchTypes t TyInt) ++ (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
matchTypeExpr env envF t (Infix _ e1 e2) = (matchTypes t TyBool) ++ (matchTypes t1 t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
matchTypeExpr env envF t (Let (nameVar, typeVar) e1 e2) = (matchTypes t t2) ++ (matchTypes typeVar t1) ++ (getErrors envAct envF e1) ++ (getErrors envAct envF e2)
  where t1 = getType envAct envF e1 
        t2 = getType envAct envF e2
        envAct = (nameVar,typeVar) : env

--Funcion igual a match sin que nos restringa a un tipo inicialmente
getErrors :: Env -> [TypedFun] -> Expr -> [Error]
getErrors _ _ (Var _) = []
getErrors _ _ (BoolLit _) = []
getErrors _ _ (IntLit _) = []
getErrors env envF (If e1 e2 e3) = (matchTypes TyBool t1) ++ (matchTypes t2 t3) ++ (getErrors env envF e1) ++ (getErrors env envF e2) ++ (getErrors env envF e3)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
        t3 = getType env envF e3
getErrors env envF (App name exps)
  = if lenExps == lenArgs then (matchTypeExprS env envF fArgTypes exps)
    else [(ArgNumApp name lenArgs lenExps)] ++ (matchTypeExprS env envF fArgTypes exps)
  where lenExps = length exps
        lenArgs = cantArgs name envF
        fArgTypes = funArgTypes name envF
getErrors env envF (Infix Add e1 e2) = (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
getErrors env envF (Infix Sub e1 e2) = (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
getErrors env envF (Infix Mult e1 e2) = (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
getErrors env envF (Infix Div e1 e2) = (matchTypes TyInt t1) ++ (matchTypes TyInt t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
getErrors env envF (Infix _ e1 e2) = (matchTypes t1 t2) ++ (getErrors env envF e1) ++ (getErrors env envF e2)
  where t1 = getType env envF e1 
        t2 = getType env envF e2
getErrors env envF (Let (nameVar, typeVar) e1 e2) = (matchTypes typeVar t1) ++ (getErrors envAct envF e1) ++ (getErrors envAct envF e2)
  where t1 = getType envAct envF e1 
        t2 = getType envAct envF e2
        envAct = (nameVar,typeVar) : env


--Dado un entorno de variables, un entorno de funciones y una expresion, nos devuelve el tipo de la expresion
getType :: Env -> [TypedFun] -> Expr -> Type
getType env _ (Var name) = t
  where t = varTypeFromEnv name env
getType _ _ (BoolLit _) = TyBool
getType _ _ (IntLit _) = TyInt
getType env envF (If _ e2 _) = getType env envF e2
getType env envF (Infix Add e1 e2) = (TyInt)
getType env envF (Infix Sub e1 e2) = (TyInt)
getType env envF (Infix Mult e1 e2) = (TyInt)
getType env envF (Infix Div e1 e2) = (TyInt)
getType env envF (Infix _ e1 e2) = (TyBool)
getType env envF (App name _) = funType name envF
getType env envF (Let (name, t) e1 e2) = getType ((name, t):env) envF e2

--Devuelve el tipo esperado en el env de una variable dado su nombre----------------------------------
varTypeFromEnv :: Name -> Env -> Type
varTypeFromEnv _ [] = TyInt
varTypeFromEnv nameVar ((name, ty):es)
  = if nameVar == name then ty
    else varTypeFromEnv nameVar es

--Devuelve la lista de tipos de los argumentos de la funcion dado su nombre y entorno de funciones----- 
funArgTypes :: Name -> [TypedFun] -> [Type]
funArgTypes _ [] = []
funArgTypes name ((nameFun, (Sig types _)):fs)
  = if name == nameFun then types
    else funArgTypes name fs

--Dado un nombre y entorno de funciones devuelve el tipo de esa funcion--------------------------------
funType :: Name -> [TypedFun] -> Type
funType _ [] = TyBool
funType name ((nameFun, (Sig _ t)):fs)
  = if name == nameFun then t
    else funType name fs 

--Matchea todas las expresiones con todos los tipos posibles--------------------------------------------
matchTypeExprS :: Env -> [TypedFun] -> [Type] -> [Expr] -> [Error]
matchTypeExprS _ _ [] _ = []
matchTypeExprS _ _ _ [] = []
matchTypeExprS env envF (t:ts) (e:es) = (matchTypeExpr env envF t e) ++ (matchTypeExprS env envF ts es)

--Matchea una variable nameVar que deberia ser de tipo t con lo que es en verdad en el env
matchVarType :: Name -> Type -> Env -> [Error]
matchVarType _ _ [] = []
matchVarType nameVar t ((name, ty):es)
  = if nameVar == name then matchTypes t ty
    else matchVarType nameVar t es

--Matchea dos tipos, si son distintos devuelve error-------------------------------------------------------
matchTypes :: Type -> Type -> [Error]
matchTypes t1 t2 
  = if t1 == t2 then []
    else [(Expected t1 t2)]

--cantArgs: recibe el nombre de una funcion y envFun, devuelve el numero de argumentos de esa funcion------
cantArgs :: Name -> [TypedFun] -> Int
cantArgs _ [] = 0
cantArgs name ((nameF, (Sig types _)):fs)
  = if name ==  nameF then length types
    else cantArgs name fs
