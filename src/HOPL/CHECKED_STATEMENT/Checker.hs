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
module HOPL.CHECKED_STATEMENT.Checker (check, checkWith, parseToplevel, checkProgram) where

import HOPL.CHECKED_STATEMENT.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.CHECKED_STATEMENT.Environment (Env (..))
import HOPL.CHECKED_STATEMENT.Lang.Parser (ParseError, parseToplevel)
import HOPL.CHECKED_STATEMENT.Lang.Syntax (Exp (..), Pgm (..), Stmt (..))
import HOPL.CHECKED_STATEMENT.TypeEnv
import HOPL.Types (Source)

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith τ src = case result of
  Right prog -> checkProgram prog τ `seq` result
  _ -> result
  where
    result = parseToplevel src

reportUnequalTypes :: Type -> Type -> Exp -> a
reportUnequalTypes t₁ t₂ exp =
  error $
    "Types didn't match: "
      ++ show t₁
      ++ " /= "
      ++ show t₂
      ++ " in "
      ++ show (show exp)

checkProgram :: Pgm -> TypeEnvironment -> Pgm
checkProgram (Pgm s) τ = Pgm (checkStatement s τ)

checkStatement :: Stmt -> TypeEnvironment -> Stmt
checkStatement stmt τ = case stmt of
  AssignStmt var rhs ->
    let tvar = applyTenv τ var
    in let t = typeOf rhs τ
    in if t == tvar then stmt else reportUnequalTypes t tvar rhs
  PrintStmt exp -> typeOf exp τ `seq` stmt
  MultiStmt [] -> stmt
  MultiStmt (stmt : stmts) -> checkStatement stmt τ `seq` checkStatement (MultiStmt stmts) τ
  IfStmt test stmt₁ stmt₂ ->
    let t = typeOf test τ
    in if t == BoolType
      then checkStatement stmt₁ τ `seq` checkStatement stmt₂ τ
      else reportUnequalTypes BoolType t test
  WhileStmt test body ->
    let t = typeOf test τ
    in if t == BoolType
      then checkStatement body τ
      else reportUnequalTypes BoolType t test
  BlockStmt vars tys body -> checkStatement body (extendTenv' vars tys τ)

typeOf :: Exp -> TypeEnvironment -> Type
typeOf (ConstExp _) _ = IntType
typeOf (StrExp _) _ = StrType
typeOf (VarExp x) τ = applyTenv τ x
typeOf (IsZeroExp exp) τ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp τ
typeOf (NotExp exp) τ
  | t == BoolType = BoolType
  | otherwise = reportUnequalTypes BoolType t exp
  where
    t = typeOf exp τ
typeOf (DiffExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (SumExp exp₁ exp₂) τ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (ProdExp exp₁ exp₂) τ
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
typeOf (AssignExp var rhs) τ
  | t₁ /= t₂ = reportUnequalTypes IntType t₂ rhs
  | otherwise = IntType
  where
    t₁ = applyTenv τ var
    t₂ = typeOf rhs τ
typeOf (BeginExp [exp]) τ = typeOf exp τ
typeOf (BeginExp (exp : exps)) τ = typeOf exp τ `seq` typeOf (BeginExp exps) τ
typeOf (BeginExp []) τ = undefined
typeOf (NewPairExp exp₁ exp₂) τ = PairType t₁ t₂
  where
    t₁ = typeOf exp₁ τ
    t₂ = typeOf exp₂ τ
typeOf (UnpairExp var₁ var₂ rhs body) τ = typeOf body τ'
  where
    τ' = extendTenv var₂ t₂ (extendTenv var₁ t₁ τ)
    PairType t₁ t₂ = typeOf rhs τ
typeOf exp@(SetLeftExp exp₁ exp₂) τ
  | t == t₁ = IntType
  | otherwise = reportUnequalTypes t t₁ exp
  where
    t = typeOf exp₂ τ
    PairType t₁ t₂ = typeOf exp₁ τ
typeOf exp@(SetRightExp exp₁ exp₂) τ
  | t == t₂ = IntType
  | otherwise = reportUnequalTypes t t₂ exp
  where
    t = typeOf exp₂ τ
    PairType t₁ t₂ = typeOf exp₁ τ

{--- Auxiliary functions ---}
