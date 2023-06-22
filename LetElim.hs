----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List        

letElimP :: Program -> Program 
letElimP (Program defs expr) = Program (map letElimF defs) (manejarLets expr)

letElimF :: FunDef -> FunDef
letElimF (FunDef funTip vars espresion) = FunDef funTip vars (manejarLets espresion)

manejarLets :: Expr -> Expr
manejarLets (Let typedVar e1 e2) = resultado
    where 
        calculado = manejarSiCorresponde typedVar (manejarLets e1)  e2
        resultado = 
            case calculado of 
                Let variable e1Calc e2Calc -> Let variable e1Calc (manejarLets e2Calc)
                _ -> calculado
manejarLets (Var n) = Var n
manejarLets (Infix op e1 e2) = Infix op (manejarLets e1) (manejarLets e2)
manejarLets (If e1 e2 e3) = If (manejarLets e1) (manejarLets e2) (manejarLets e3)
manejarLets (App n exprs) = App n (map manejarLets exprs)
manejarLets expr = expr

-- solo se manejan aquellos lets que tienen un literal como expresion
manejarSiCorresponde :: TypedVar -> Expr -> Expr -> Expr
manejarSiCorresponde x (IntLit valor) expresion = subst (fst x) (manejarLets expresion) (IntLit valor)
manejarSiCorresponde x (BoolLit valor) expresion = subst (fst x) (manejarLets expresion) (BoolLit valor)
manejarSiCorresponde x e1 e2 =  Let x e1 e2

subst :: Name -> Expr -> Expr -> Expr
subst varName (Var n) (IntLit intLit)
  | n == varName = IntLit intLit
  | otherwise = Var n
subst varName (Var n) (BoolLit boolLit)
  | n == varName = BoolLit boolLit
  | otherwise = Var n
subst _ (IntLit n) _ = IntLit n
subst _ (BoolLit b) _ = BoolLit b
subst varName (Infix op e1 e2) intVal = Infix op (subst varName e1 intVal) (subst varName e2 intVal)
subst varName (If cond et ef) intVal = If (subst varName cond intVal) (subst varName et intVal) (subst varName ef intVal)
subst varName (Let typedVar e1 e2) intVal = if (fst typedVar /= varName) then Let typedVar (subst varName e1 intVal) (subst varName e2 intVal) else Let typedVar e1 e2
subst varName (App nombre expresiones) intVal = App nombre (map (\expresion -> subst varName expresion intVal) expresiones)





