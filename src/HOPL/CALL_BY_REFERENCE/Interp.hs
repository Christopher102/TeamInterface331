{-
 -  HOPL/CALL_BY_REFERENCE/Interp.hs
 -
 -  Reference implementation of the toy language CALL_BY_REFERENCE from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_REFERENCE.Interp
  ( interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.CALL_BY_REFERENCE.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.CALL_BY_REFERENCE.Environment (Env (..))
import HOPL.CALL_BY_REFERENCE.Lang.Parser (ParseError, parseToplevel)
import HOPL.CALL_BY_REFERENCE.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.CALL_BY_REFERENCE.Store (Store, deref, emptyStore, left, makePair, newref, right, setLeft, setRight, setref)
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
valueOf (VarExp var) ρ σ = Answer (deref addr σ) σ
  where
    addr = applyEnv ρ var
valueOf (ConstExp n) _ σ = Answer (NumVal n) σ
valueOf (IsZeroExp exp₁) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf exp₁ ρ σ
valueOf (DiffExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₂
  where
    ρ' = extendEnv x addr ρ
    (addr, σ₂) = newref v σ₁
    Answer v σ₁ = valueOf rhs ρ σ
valueOf (LetrecExp pname param pbody body) ρ σ = valueOf body ρ' σ₂
  where
    (addr, σ₁) = newref undefined σ
    ρ' = extendEnv pname addr ρ
    σ₂ = setref addr (ProcVal (ClosedProcedure param pbody ρ')) σ₁
valueOf (IfExp exp₁ exp₂ exp₃) ρ σ = valueOf exp' ρ σ₁
  where
    Answer q σ₁ = valueOf exp₁ ρ σ
    exp' = case q of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
valueOf (ProcExp x body) ρ σ = Answer (ProcVal (ClosedProcedure x body ρ)) σ
valueOf (CallExp rator rand) ρ σ = applyProcedure (expvalToProc f) addr σ₂
  where
    Answer f σ₁ = valueOf rator ρ σ
    (addr, σ₂) = valueOfOperand rand ρ σ₁
valueOf (AssignExp var rhs) ρ σ = Answer (NumVal 42) σ₂
  where
    Answer rval σ₁ = valueOf rhs ρ σ
    σ₂ = setref (applyEnv ρ var) rval σ₁
valueOf (BeginExp []) _ _ = undefined
valueOf (BeginExp (exp : exps)) ρ σ
  | null exps = Answer val σ₁
  | otherwise = valueOf (BeginExp exps) ρ σ₁
  where
    Answer val σ₁ = valueOf exp ρ σ
valueOf (NewPairExp exp₁ exp₂) ρ σ = Answer (MutPairVal pr) σ₃
  where
    Answer v₁ σ₁ = valueOf exp₁ ρ σ
    Answer v₂ σ₂ = valueOf exp₂ ρ σ₁
    (pr, σ₃) = makePair σ₂ v₁ v₂
valueOf (LeftExp exp) ρ σ = Answer v σ₁
  where
    Answer (MutPairVal pr) σ₁ = valueOf exp ρ σ
    v = left σ₁ pr
valueOf (RightExp exp) ρ σ = Answer v σ₁
  where
    Answer (MutPairVal pr) σ₁ = valueOf exp ρ σ
    v = right σ₁ pr
valueOf (SetLeftExp lhs rhs) ρ σ = Answer (NumVal 42) σ₃
  where
    Answer (MutPairVal pr) σ₁ = valueOf lhs ρ σ
    Answer rval σ₂ = valueOf rhs ρ σ₁
    σ₃ = setLeft σ₂ pr rval
valueOf (SetRightExp lhs rhs) ρ σ = Answer (NumVal 42) σ₃
  where
    Answer (MutPairVal pr) σ₁ = valueOf lhs ρ σ
    Answer rval σ₂ = valueOf rhs ρ σ₁
    σ₃ = setRight σ₂ pr rval

{- Auxiliary procedure to support call-by-reference semantics -}
valueOfOperand :: Exp -> Environment -> Store -> (DenVal, Store)
valueOfOperand (VarExp var) ρ σ = (applyEnv ρ var, σ)
valueOfOperand exp ρ σ = newref v σ₁
  where
    Answer v σ₁ = valueOf exp ρ σ

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> Store -> Answer
applyProcedure (ClosedProcedure x body ρ) arg σ = valueOf body (extendEnv x arg ρ) σ
applyProcedure _ _ _ = undefined
