{-
 -  HOPL/CHECKED/Checker.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the static type checker implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED.Checker (check, checkWith, parseToplevel, typeOfProgram) where

import HOPL.CHECKED.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.CHECKED.Environment (Env (..))
import HOPL.CHECKED.Lang.Parser (ParseError, parseToplevel)
import HOPL.CHECKED.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.CHECKED.TypeEnv
import HOPL.Types (Source)

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith τ src = case result of
  Right prog -> typeOfProgram prog τ `seq` result
  _ -> result
  where
    result = parseToplevel src

reportUnequalTypes :: Type -> Type -> Exp -> Type
reportUnequalTypes t₁ t₂ exp =
  error $
    "Types didn't match: "
      ++ show t₁
      ++ " /= "
      ++ show t₂
      ++ " in "
      ++ show (show exp)

typeOfProgram :: Pgm -> TypeEnvironment -> Type
typeOfProgram (Pgm e) τ = typeOf e τ

typeOf :: Exp -> TypeEnvironment -> Type
typeOf (ConstExp _) _ = IntType
typeOf (VarExp x) τ = applyTenv τ x
typeOf (IsZeroExp exp) τ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp τ
typeOf (DiffExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (IfExp exp₁ exp₂ exp₃) τ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | t₂ /= t₃ = reportUnequalTypes t₂ t₃ exp₂
  | otherwise = t₂
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
    t₃ = typeOf exp₃ τ
typeOf (LetExp var rhs body) τ = typeOf body τ'
  where
    τ' = extendTenv var t τ
    t = typeOf rhs τ
typeOf (LetrecExp tres pname param targ pbody body) τ
  | tres' == tres = typeOf body τ'
  | otherwise = reportUnequalTypes tres tres' pbody
  where
    τ' = extendTenv pname (ProcType targ tres) τ
    tres' = typeOf pbody (extendTenv param targ τ')
typeOf (ProcExp param targ body) τ = ProcType targ tres
  where
    tres = typeOf body τ'
    τ' = extendTenv param targ τ
typeOf (CallExp rator rand) τ
  | targ == targ' = tres
  | otherwise = reportUnequalTypes targ targ' rand
  where
    ProcType targ tres = typeOf rator τ
    targ' = typeOf rand τ

{--- Auxiliary functions ---}
