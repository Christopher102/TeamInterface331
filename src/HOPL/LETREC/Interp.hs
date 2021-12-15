{-
 -  HOPL/LETREC/Interp.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Interp
  ( interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.LETREC.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.LETREC.Environment (Env (..))
import HOPL.LETREC.Lang.Parser (ParseError, parseToplevel)
import HOPL.LETREC.Lang.Syntax
import HOPL.LETREC.Prelude
import HOPL.Types (Source)
import Prelude hiding (exp)

{- top-level interpreter routines -}

interp :: Source -> Either ParseError ExpVal
interp = interpWith preludeEnv

interpWith' :: Environment -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Environment -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- Shim for pre-interpreting the prelude definitions -}

-- Compiles in last-to-first order, see Prelude.hs for more info.
compileOne :: Def -> Environment -> Environment
compileOne (name, src) ρ = extendEnv name val ρ
  where
    val = fromRight undefined $ interpWith ρ src

preludeEnv :: Environment
preludeEnv = foldr compileOne emptyEnv prelude

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> ExpVal
-- Variable reference
valueOf (VarExp var) ρ = applyEnv ρ var
-- Integer literal
valueOf (ConstExp n) _ = NumVal n
-- Boolean literals
valueOf TrueExp ρ = BoolVal True
valueOf FalseExp ρ = BoolVal False
-- Unary operation
valueOf (UnaryExp op exp₁) ρ = valueOfUnaryOp op exp₁ ρ
valueOf (BinaryExp op exp₁ exp₂) ρ = valueOfBinaryOp op exp₁ exp₂ ρ
-- List constructors
valueOf EmptyExp ρ = ListVal []
valueOf (ListExp exps) ρ = ListVal vs
  where
    vs = map (`valueOf` ρ) exps
-- Variable declarations
valueOf (LetExp var rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv var v ρ
    v = valueOf rhs ρ
valueOf (LetrecExp pname param pbody body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv pname (ProcVal (OpenProcedure param pbody)) ρ
valueOf (UnpackExp vars rhs body) ρ = undefined -- TODO implement semantics for unpack
-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
-- Function definition
valueOf (ProcExp param body) ρ = ProcVal (ClosedProcedure param body ρ)
-- Function call
valueOf (CallExp rator rand) ρ = applyProcedure f arg
  where
    arg = valueOf rand ρ
    f = expvalToProc (valueOf rator ρ)

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> ExpVal
applyProcedure (ClosedProcedure x body ρ) arg = valueOf body (extendEnv x arg ρ)
applyProcedure _ _ = undefined

{- Built-in operations -}
valueOfUnaryOp :: UnaryOp -> Exp -> Environment -> ExpVal
valueOfUnaryOp op exp ρ = case op of
  IsZero -> BoolVal (n == 0)
  IsNeg -> BoolVal (n < 0)
  IsPos -> BoolVal (n > 0)
  Minus -> NumVal (- n)
  Not -> BoolVal (not $ expvalToBool v)
  IsNull -> BoolVal (null vs)
  Car -> head vs
  Cdr -> ListVal (tail vs)
  where
    vs = expvalToList v
    n = expvalToNum v
    v = valueOf exp ρ

valueOfBinaryOp :: BinaryOp -> Exp -> Exp -> Environment -> ExpVal
valueOfBinaryOp op exp₁ exp₂ ρ = case op of
  Diff -> NumVal (n₁ - n₂)
  Plus -> NumVal (n₁ + n₂)
  Times -> NumVal (n₁ * n₂)
  Divides -> NumVal (n₁ `div` n₂)
  Mod -> NumVal (n₁ `mod` n₂)
  Equal -> BoolVal (v₁ == v₂)
  NotEqual -> BoolVal (v₁ /= v₂)
  Less -> BoolVal (n₁ < n₂)
  LessEqual -> BoolVal (n₁ < n₂)
  Greater -> BoolVal (n₁ < n₂)
  GreaterEqual -> BoolVal (n₁ < n₂)
  And -> BoolVal (q₁ && q₂)
  Or -> BoolVal (q₁ || q₂)
  Cons -> ListVal (v₁ : expvalToList v₂)
  where
    q₁ = expvalToBool v₁
    q₂ = expvalToBool v₂
    n₁ = expvalToNum v₁
    n₂ = expvalToNum v₂
    v₁ = valueOf exp₁ ρ
    v₂ = valueOf exp₂ ρ
