{-
 -  HOPL/CHECKED/Interp.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED_STATEMENT.Interp
  ( checkAndInterp,
    checkAndInterpWith,
    interpWith,
  )
where

import Data.Either (fromRight)
import HOPL.CHECKED_STATEMENT.Checker
import HOPL.CHECKED_STATEMENT.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.CHECKED_STATEMENT.Environment (Env (..))
import HOPL.CHECKED_STATEMENT.Lang.Parser (ParseError, parseToplevel)
import HOPL.CHECKED_STATEMENT.Lang.Syntax (Exp (..), Pgm (..), Stmt (..))
import HOPL.CHECKED_STATEMENT.Store (Store, deref, emptyStore, left, makePair, newref, right, setLeft, setRight, setref)
import HOPL.CHECKED_STATEMENT.TypeEnv (TEnv (..), TypeEnvironment)
import HOPL.Types (Source)
import Prelude hiding (exp)

{- Evaluating a program yields an "answer" - a value and a resulting state. -}
data Answer = Answer {getVal :: ExpVal, getStore :: Store}

{- top-level interpreter routines -}

checkAndInterp :: Source -> Either ParseError (IO Store)
checkAndInterp = checkAndInterpWith emptyTenv emptyEnv emptyStore

checkAndInterpWith :: TypeEnvironment -> Environment -> Store -> Source -> Either ParseError (IO Store)
checkAndInterpWith τ ρ σ src = flip (`resultOfProgram` ρ) σ <$> checkWith τ src

interp :: Source -> Either ParseError (IO Store)
interp = interpWith emptyEnv emptyStore

interpWith' :: Environment -> Store -> Source -> IO Store
interpWith' ρ σ = fromRight undefined . interpWith ρ σ

interpWith :: Environment -> Store -> Source -> Either ParseError (IO Store)
interpWith ρ σ src = flip (`resultOfProgram` ρ) σ <$> parseToplevel src

{- semantic reduction of a program -}

resultOfProgram :: Pgm -> Environment -> Store -> IO Store
resultOfProgram (Pgm stmt) ρ σ = resultOf stmt ρ σ

{- semantic reductions for statements -}

resultOf :: Stmt -> Environment -> Store -> IO Store
resultOf (AssignStmt var rhs) ρ σ = return σ₂
  where
    Answer rval σ₁ = valueOf rhs ρ σ
    addr = applyEnv ρ var
    σ₂ = setref addr rval σ₁
resultOf (PrintStmt exp) ρ σ = do
  case v of
    MutPairVal pr -> print (left σ₁ pr, right σ₁ pr)
    _ -> print v
  return σ₁
  where
    Answer v σ₁ = valueOf exp ρ σ
resultOf (MultiStmt []) ρ σ = return σ
resultOf (MultiStmt (stmt : stmts)) ρ σ = do
  σ₁ <- resultOf stmt ρ σ
  resultOf (MultiStmt stmts) ρ σ₁
resultOf (IfStmt test conseq altern) ρ σ = if q then σ₂ else σ₃
  where
    Answer (BoolVal q) σ₁ = valueOf test ρ σ
    σ₂ = resultOf conseq ρ σ₁
    σ₃ = resultOf altern ρ σ₁
resultOf stmt@(WhileStmt test body) ρ σ =
  if q
    then do
      σ₂ <- resultOf body ρ σ₁
      resultOf stmt ρ σ₂
    else return σ₁
  where
    Answer (BoolVal q) σ₁ = valueOf test ρ σ

-- Recursive scoping - later declarations may refer to earlier ones
resultOf (BlockStmt [] [] stmt) ρ σ = resultOf stmt ρ σ
resultOf (BlockStmt (var : vars) (ty : tys) stmt) ρ σ = resultOf (BlockStmt vars tys stmt) ρ' σ₁
  where
    ρ' = extendEnv var addr ρ -- extend the environment with the variable bound to the new location
    (addr, σ₁) = newref undefined σ -- allocate an uninitialized location in the store
resultOf (BlockStmt _ _ stmt) _ _ = undefined

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> Store -> Answer
-- Variable reference
valueOf (VarExp x) ρ σ = Answer (deref addr σ) σ
  where
    addr = applyEnv ρ x
-- Integer literal
valueOf (ConstExp n) _ σ = Answer (NumVal n) σ
-- String literal
valueOf (StrExp s) _ σ = Answer (StrVal s) σ
-- Arithmetic/numeric predicates
valueOf (IsZeroExp exp₁) ρ σ = Answer (BoolVal (n == 0)) σ₁
  where
    Answer (NumVal n) σ₁ = valueOf exp₁ ρ σ
valueOf (NotExp exp₁) ρ σ = Answer (BoolVal (not q)) σ₁
  where
    Answer (BoolVal q) σ₁ = valueOf exp₁ ρ σ
-- Arithmetic operators
valueOf (DiffExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ - n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (SumExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ + n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
valueOf (ProdExp exp₁ exp₂) ρ σ = Answer (NumVal (n₁ * n₂)) σ₂
  where
    Answer (NumVal n₁) σ₁ = valueOf exp₁ ρ σ
    Answer (NumVal n₂) σ₂ = valueOf exp₂ ρ σ₁
-- Variable declarations
valueOf (LetExp x rhs body) ρ σ = valueOf body ρ' σ₂
  where
    ρ' = extendEnv x addr ρ
    (addr, σ₂) = newref v σ₁
    Answer v σ₁ = valueOf rhs ρ σ
valueOf (LetrecExp _ pname param _ pbody body) ρ σ = valueOf body ρ' σ₂
  where
    (addr, σ₁) = newref undefined σ
    ρ' = extendEnv pname addr ρ
    σ₂ = setref addr (ProcVal (ClosedProcedure param pbody ρ')) σ₁
-- Control expressions
valueOf (IfExp exp₁ exp₂ exp₃) ρ σ = valueOf exp' ρ σ₁
  where
    Answer q σ₁ = valueOf exp₁ ρ σ
    exp' = case q of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
-- Function definition
valueOf (ProcExp params _ body) ρ σ = Answer (ProcVal (ClosedProcedure params body ρ)) σ
-- Function call
valueOf (CallExp rator rand) ρ σ = applyProcedure (expvalToProc f) addr σ₃
  where
    Answer f σ₁ = valueOf rator ρ σ
    Answer v σ₂ = valueOf rand ρ σ₁
    (addr, σ₃) = newref v σ₂
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
valueOf (UnpairExp var₁ var₂ rhs body) ρ σ = valueOf body ρ₂ σ₁
  where
    ρ₂ = extendEnv var₂ loc₂ ρ₁
    ρ₁ = extendEnv var₁ loc₁ ρ
    Answer (MutPairVal (loc₁, loc₂)) σ₁ = valueOf rhs ρ σ
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

{- Auxiliary function for applying procedure values -}
applyProcedure :: Procedure -> DenVal -> Store -> Answer
applyProcedure (ClosedProcedure param body ρ) arg σ = valueOf body ρ' σ
  where
    ρ' = extendEnv param arg ρ
applyProcedure _ _ _ = undefined
