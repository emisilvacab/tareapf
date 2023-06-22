----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax


import Data.List

getTipo :: Type -> String
getTipo _ = "int"

asociarVariable :: (Type, Name) -> String
asociarVariable (tipo, nombre) = (getTipo tipo) ++ " " ++ genNombre nombre ++ ","

dropLast :: String -> String
dropLast [] = ""
dropLast [x] = ""
dropLast (x: xs) = concat [[x], (dropLast xs)]

getBoolVal :: Bool -> String
getBoolVal True = "1"
getBoolVal False = "0"

genOp :: Op -> String
genOp Add = " + "
genOp Sub = " - "
genOp Mult = " * "
genOp Div = "/"
genOp Eq = "=="
genOp NEq = "!="
genOp GTh = ">"
genOp LTh = "<"
genOp GEq = ">="
genOp LEq = "<="

genNombre :: String -> String
genNombre t = "_" ++ t

getExpresionFromFunDef :: FunDef -> Expr
getExpresionFromFunDef (FunDef _ _ expr) = expr

buscarLetsEnCadaFuncion :: [FunDef] -> String
buscarLetsEnCadaFuncion funs = undefined

eliminarUltimaComa :: String -> String
eliminarUltimaComa s = if (last s == ',') then reverse (tail (reverse s)) else s


-- CODE GENERATOR
-- TODO: Print del main
genProgram :: Program -> String
genProgram (Program defs main) = "#include <stdio.h>" ++ concat (map (\def@(FunDef typedFun vars expr) -> genFirmaFuncion typedFun vars ++ fst (buscarLet (getExpresionFromFunDef def) 0 "") ++ genCuerpoFuncion expr 0) defs) ++  "int main() {" ++ fst (buscarLet main 0 "") ++ "\nprintf(\"%d\\n\"," ++ fst (genExpresion 0 main) ++ "); }" 

genCuerpoFuncion :: Expr -> Integer-> String
genCuerpoFuncion expresion contador = concat ["\nreturn (", fst (genExpresion contador expresion), "); };"]

genFirmaFuncion :: TypedFun -> [Name] -> String
genFirmaFuncion (nombre, (Sig tipos tipoRetorno)) parametros = concat ["\n", getTipo tipoRetorno, " ", genNombre nombre, "(" , eliminarUltimaComa (concat (map asociarVariable (zip tipos parametros))), "){"] --revisa que pasa con las comas cuando hay muchos parametros

genExpresion :: Integer -> Expr -> (String, Integer)
genExpresion contador (Var n) = (genNombre n, contador)
genExpresion contador (IntLit val) = (show val, contador)
genExpresion contador (BoolLit val) = (getBoolVal val, contador)
genExpresion contador (Infix op e1 e2) = (concat ["(", fst resuE1, genOp op, fst resuE2, ")"], snd resuE2)
    where
        resuE1 = (genExpresion contador e1)
        resuE2 = genExpresion (snd resuE1) e2
genExpresion contador (If cond et ef) = (concat ["(", fst resuCond, "?", fst resuEt, ":", fst resuEf, ")"], snd resuEf)
    where
        resuCond = genExpresion contador cond
        resuEt = genExpresion (snd resuCond) et
        resuEf = genExpresion (snd resuEt) ef
genExpresion contador (Let (x, _) e1 e2) = (concat ["_let", (show contador), "(", textoE1, ")"], contadorFinal)
    where
        (textoE1, contadorFinal) = genExpresion (contador + 1) e1
genExpresion contador (App nombre expresiones) = (genNombre nombre ++ "(" ++ dropLast texto ++ ")", contadorFinal)
    where
        (texto, contadorFinal) = generarExpresiones contador expresiones

generarExpresiones :: Integer -> [Expr] -> (String, Integer)
generarExpresiones contador [] = ("", contador)
generarExpresiones contador (e:es) =
  let (expresionGenerada, contadorActualizado) = genExpresion contador e
      (expresionesGeneradas, nuevoContador) = generarExpresiones contadorActualizado es
  in (expresionGenerada ++ "," ++ expresionesGeneradas, nuevoContador)


    --firmasE1 ++ definirLet contadorActualizado x e1 e2 ++ cuerposE1, contadorActualizado + 1)
    --  definirFirmaLet contadorFinal x ++ buscarLet e1 contador textoActual ++ genCuerpoFuncion buscarLet e2 contadorActualizado textoActual
buscarLet :: Expr -> Integer -> String -> (String, Integer)
buscarLet (Let x e1 e2) contador textoActual = (letsEnE1 ++ firmaActual ++ definiciones ++ genCuerpoFuncion e2 primerContador, primerContador + 1)
    where
        (letsEnE1, contadorPostE1) = buscarLet e1 contador textoActual
        (definiciones, primerContador) = buscarLet e2 contadorPostE1 ""
        firmaActual = definirFirmaLet primerContador x
buscarLet (Var _) contador textoActual = (textoActual, contador)
buscarLet (IntLit val) contador textoActual = (textoActual, contador)
buscarLet (BoolLit val) contador textoActual = (textoActual, contador)
buscarLet (Infix op e1 e2) contador textoActual = (concat [textoActual, texto, texto2], contFinal)
    where 
        (texto, contAct) = buscarLet e1 contador textoActual
        (texto2, contFinal) = buscarLet e2 contAct textoActual
buscarLet (If cond et ef) contador textoActual = (concat [textoActual, texto, texto2], contFinal)
    where 
        (texto, contAct) = buscarLet et contador textoActual
        (texto2, contFinal) = buscarLet ef contAct textoActual
buscarLet (App nombre expresiones) contador textoActual  = foldl (\(text, acum) expresion -> buscarLet expresion acum textoActual) (textoActual, contador) expresiones

-- define la funcion para un let
-- definirLet :: Integer -> TypedVar -> Expr -> Expr -> String
-- definirLet contador (varName, tipoVar) _ e2 = genFirmaFuncion ("let" ++ show contador, Sig [tipoVar] tipoVar) [varName] ++ genCuerpoFuncion e2

definirFirmaLet :: Integer -> TypedVar -> String
definirFirmaLet contador (varName, tipoVar) = genFirmaFuncion ("let" ++ show contador, Sig [tipoVar] tipoVar) [varName]


