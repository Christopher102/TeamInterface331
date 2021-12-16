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
module HOPL.STATPY.Checker (check, checkWith) where

import HOPL.STATPY.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.STATPY.Environment (Env (..))
import HOPL.STATPY.Lang.Parser (ParseError, parseToplevel)
import HOPL.STATPY.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.STATPY.TypeEnv
import HOPL.STATPY.Types (Source)

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith ρ src = case result of
  Right prog -> typeOfProgram prog ρ `seq` result
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
typeOfProgram (Pgm e) ρ = typeOf e ρ

typeOf :: Exp -> TypeEnvironment -> Type
typeOf (ConstExp _) _ = IntType
typeOf (VarExp x) ρ = applyTenv ρ x




typeOf (IsZeroExp exp) ρ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp ρ

--Diff Exp
typeOf (DiffExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ


-- Add Exp
typeOf (AddExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

-- Mult Exp 
typeOf (MultExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--Div Exp
typeOf (DivExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

-- ExpoExp
typeOf (ExpoExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

-- ModExp
typeOf (ModExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--Sqrt Exp
typeOf (SqrtExp exp₁ ) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
  

-- GreaterExp
typeOf (GreaterExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--Less Exp 
typeOf (LessExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--GreatEqExp
typeOf (GreatEqExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--LessEqExp
typeOf (LessEqExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--EqualExp
typeOf (EqualExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ

--NotExp
typeOf (NotExp exp₁) ρ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | otherwise = BoolType
  where
    t₁ = typeOf exp₁ ρ

typeOf (IfExp exp₁ exp₂ exp₃) ρ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | t₂ /= t₃ = reportUnequalTypes t₂ t₃ exp₂
  | otherwise = t₂
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ
    t₃ = typeOf exp₃ ρ

typeOf (LetExp var rhs body) ρ = typeOf body ρ'
  where
    ρ' = extendTenv var t ρ
    t = typeOf rhs ρ

typeOf (LetrecExp tres pname param targ pbody body) ρ
  | tres' == tres = typeOf body ρ'
  | otherwise = reportUnequalTypes tres tres' pbody
  where
    ρ' = extendTenv pname (ProcType targ tres) ρ
    tres' = typeOf pbody (extendTenv param targ ρ')

typeOf (DefExp param targ body) ρ = DefType targ tres
  where
    tres = typeOf body ρ'
    ρ' = extendTenv param targ ρ
typeOf (CallExp rator rand) ρ
  | targ == targ' = tres
  | otherwise = reportUnequalTypes targ targ' rand
  where
    DefType targ tres = typeOf rator ρ
    targ' = typeOf rand ρ

{--- Auxiliary functions ---}
