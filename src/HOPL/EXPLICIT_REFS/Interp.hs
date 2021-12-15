{-
 -  HOPL/EXPLICIT_REFS/Interp.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.EXPLICIT_REFS.Interp
  ( interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.EXPLICIT_REFS.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.EXPLICIT_REFS.Environment (Env (..))
import HOPL.EXPLICIT_REFS.Lang.Parser (ParseError, parseToplevel)
import HOPL.EXPLICIT_REFS.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.EXPLICIT_REFS.Store (Store, deref, emptyStore, newref, setref)
import HOPL.Types (Source)
import Prelude hiding (exp)

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
data Answer = Answer {getVal :: ExpVal, getStore :: Store}

{- top-level interpreter routines -}

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv emptyStore

interpWith' :: Environment -> Store -> Source -> ExpVal
interpWith' ρ σ = fromRight undefined . interpWith ρ σ

interpWith :: Environment -> Store -> Source -> Either ParseError ExpVal
interpWith ρ σ src = flip (`valueOfProgram` ρ) σ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> Store -> ExpVal
valueOfProgram (Pgm exp) ρ σ = getVal (valueOf exp ρ σ)

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> Store -> Answer
valueOf (VarExp x) ρ σ = Answer (applyEnv ρ x) σ
valueOf (ConstExp n) _ σ = Answer (NumVal n) σ
valueOf (IsZeroExp exp₁) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf exp₁ ρ σ
valueOf (DiffExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₁
  where
    ρ' = extendEnv x v ρ
    Answer v σ₁ = valueOf rhs ρ σ
valueOf (LetrecExp pname param pbody body) ρ σ = valueOf body ρ' σ
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
valueOf (IfExp exp₁ exp₂ exp₃) ρ σ = valueOf exp' ρ σ₁
  where
    Answer q σ₁ = valueOf exp₁ ρ σ
    exp' = case q of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
valueOf (ProcExp x body) ρ σ = Answer (ProcVal (ClosedProcedure x body ρ)) σ
valueOf (CallExp rator rand) ρ σ = applyProcedure (expvalToProc f) arg σ₂
  where
    Answer f σ₁ = valueOf rator ρ σ
    Answer arg σ₂ = valueOf rand ρ σ₁
valueOf (NewrefExp rhs) ρ σ = Answer (RefVal addr) σ₂
  where
    Answer val σ₁ = valueOf rhs ρ σ
    (addr, σ₂) = newref val σ₁
valueOf (DerefExp rhs) ρ σ = Answer val σ₁
  where
    Answer ref σ₁ = valueOf rhs ρ σ
    addr = expvalToRef ref
    val = deref addr σ₁
valueOf (SetrefExp lhs rhs) ρ σ = Answer (NumVal 42) σ₃
  where
    Answer lval σ₁ = valueOf lhs ρ σ
    Answer rval σ₂ = valueOf rhs ρ σ₁
    σ₃ = setref (expvalToRef lval) rval σ₂
valueOf (BeginExp []) _ _ = undefined
valueOf (BeginExp (exp : exps)) ρ σ
  | null exps = Answer val σ₁
  | otherwise = valueOf (BeginExp exps) ρ σ₁
  where
    Answer val σ₁ = valueOf exp ρ σ

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> Store -> Answer
applyProcedure (ClosedProcedure x body ρ) arg σ = valueOf body (extendEnv x arg ρ) σ
applyProcedure _ _ _ = undefined
