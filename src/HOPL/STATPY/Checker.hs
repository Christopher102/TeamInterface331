{-
 -  HOPL/STATPY/LANG/Checker.hs
 -
 -  This module provides the static type checker implementation.
 -
 -  Author: Brandon Alker, Christopher Fioti, and Nicholas Petrilli
 -}
module HOPL.STATPY.Checker (check, checkWith) where

import HOPL.STATPY.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.STATPY.Environment (Env (..))
import HOPL.STATPY.Lang.Parser (ParseError, parseToplevel)
import HOPL.STATPY.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.STATPY.TypeEnv
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

typeOf (IfExp exp₁ exp₂ exp₃) τ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | t₂ /= t₃ = reportUnequalTypes t₂ t₃ exp₂
  | otherwise = t₂
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
    t₃ = typeOf exp₃ τ

typeOf (DefExp targ param body) τ = DefType targ tres
  where
    tres = typeOf body τ'
    τ' = extendTenv param targ τ

typeOf (CallExp rator rand) τ
  | targ == targ' = tres
  | otherwise = reportUnequalTypes targ targ' rand
  where
    DefType targ tres = typeOf rator τ
    targ' = typeOf rand τ

--Diff Exp
typeOf (DiffExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

-- Add Exp
typeOf (AddExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

-- Mult Exp 
typeOf (MultExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--Div Exp
typeOf (DivExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

-- ExpoExp
typeOf (ExpoExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

-- ModExp
typeOf (ModExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--Sqrt Exp
typeOf (SqrtExp exp₁ ) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
  
-- GreaterExp
typeOf (GreaterExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--Less Exp 
typeOf (LessExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--GreatEqExp
typeOf (GreatEqExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--LessEqExp
typeOf (LessEqExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--EqualExp
typeOf (EqualExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--NotEqualExp
typeOf (NotEqualExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ

--NotExp
typeOf (NotExp exp₁) τ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | otherwise = BoolType
  where
    t₁ = typeOf exp₁ τ

--IsZeroExp
typeOf (IsZeroExp exp) τ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp τ

--EmptyList
typeOf (EmptyListExp t) τ = t 

--LetExp
typeOf (LetExp t id exp) τ = t

--ListExp
typeOf (ListExp rands) τ
  | t₂ /= ListType = reportUnequalTypes ListType t₂ (ListExp(tail rands))
  | otherwise = ListType 
  where
    t₁ = typeOf (head rands) τ
    t₂ = typeOf (ListExp(tail rands)) τ

typeOf (ConstExp _) _ = IntType
typeOf (VarExp x) τ = applyTenv τ x