{-
 -  HOPL/CHECKED/Interp.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Authors: Brandon Alker, Nick Petrilli, and Christopher Fioti
 -}
module HOPL.STATPY.Interp
  ( checkAndInterp,
    interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.STATPY.Checker
import HOPL.STATPY.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.STATPY.Environment (Env (..))
import HOPL.STATPY.Lang.Parser (ParseError, parseToplevel)
import HOPL.STATPY.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.STATPY.TypeEnv (TEnv (..), TypeEnvironment)
import HOPL.STATPY.Types (Source)
import Prelude hiding (exp)

{- top-level interpreter routines -}

checkAndInterp :: Source -> Either ParseError ExpVal
checkAndInterp = checkAndInterpWith emptyTenv emptyEnv

checkAndInterpWith :: TypeEnvironment -> Environment -> Source -> Either ParseError ExpVal
checkAndInterpWith τ ρ src = flip valueOfProgram ρ <$> checkWith τ src

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv

interpWith' :: Environment -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Environment -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> ExpVal


-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃

-- Function definition
valueOf (DefExp id param body) ρ = DefVal (ClosedProcedure param body ρ)

-- Function call
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToDef (valueOf rator ρ)

-- DiffExp
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

-- Add Exp
valueOf (AddExp exp₁ exp₂) ρ = NumVal (n₁ + n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
-- Mult Exp
valueOf (MultExp exp₁ exp₂) ρ = NumVal (n₁ * n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
-- Div Exp
valueOf (DivExp exp₁ exp₂) ρ = NumVal (n₁ `div` n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

-- Expo Exp
valueOf (ExpoExp exp₁ exp₂) ρ = NumVal (n₁ ^ n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

-- ModExp
valueOf (ModExp exp₁ exp₂) ρ = NumVal (n₁ `mod` n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--Sqrt Exp 
valueOf (SqrtExp exp₁) ρ = FloatVal (sqrt n)
  where
    FloatVal n = valueOf exp₁ ρ

-- Greater Exp
valueOf (GreaterExp exp₁ exp₂) ρ = BoolVal (n₁ > n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--Less Exp
valueOf (LessExp exp₁ exp₂) ρ = BoolVal (n₁ < n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--GreatEqExp
valueOf (GreatEqExp exp₁ exp₂) ρ = BoolVal (n₁ >= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--LessEqExp
valueOf (LessEqExp exp₁ exp₂) ρ = BoolVal (n₁ <= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--EqualExp
valueOf (EqualExp exp₁ exp₂) ρ = BoolVal (n₁ == n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--NotEqualExp
valueOf (NotEqualExp exp₁ exp₂) ρ = BoolVal (n₁ /= n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ

--NotExp
valueOf (NotExp exp₁) ρ = BoolVal (not n₁)
  where
    BoolVal n₁ = valueOf exp₁ ρ

-- IsZeroExp
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ

-- EmptyListExp
valueOf (EmptyListExp t) ρ = ListVal []
-- ListExp
valueOf (ListExp rands) ρ = ListVal vs
  where
    vs = map (`valueOf` ρ) rands

-- Integer literal
valueOf (ConstExp n) _ = NumVal n

-- Variable reference
valueOf (VarExp var) ρ = applyEnv ρ var

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined
