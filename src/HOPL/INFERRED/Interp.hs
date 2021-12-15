{-
 -  HOPL/INFERRED/Interp.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Interp
  ( checkAndInterp,
    checkAndInterpWith,
    interpWith,
  )
where

import Data.Either (fromRight)
import HOPL.INFERRED.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.INFERRED.Environment (Env (..))
import HOPL.INFERRED.Inferrer
import HOPL.INFERRED.Lang.Parser (ParseError, parseToplevel)
import HOPL.INFERRED.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.INFERRED.TypeEnv (TEnv (..), TypeEnvironment)
import HOPL.Types (Source)
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
-- Variable reference
valueOf (VarExp var) ρ = applyEnv ρ var
-- Integer literal
valueOf (ConstExp n) _ = NumVal n
-- Arithmetic/numeric predicates
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ
-- Arithmetic operators
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
-- Variable declarations
valueOf (LetExp var rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv var v ρ
    v = valueOf rhs ρ
valueOf (LetrecExp _ pname param _ pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
-- Function definition
valueOf (ProcExp param _ body) ρ = ProcVal (ClosedProcedure param body ρ)
-- Function call
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToProc (valueOf rator ρ)

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined
