{-
 -  HOPL/INFERRED/Checker.hs
 -
 -  Reference implementation of the toy language INFERRED by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Inferrer (check, checkWith, parseToplevel, typeOfProgram) where

import HOPL.INFERRED.Environment
import HOPL.INFERRED.Lang.Parser
import HOPL.INFERRED.Lang.Parser (ParseError, parseToplevel)
import HOPL.INFERRED.Lang.Syntax
import HOPL.INFERRED.Substitution
import HOPL.INFERRED.Substitution (Substitution)
import HOPL.INFERRED.Type
import HOPL.INFERRED.Type (OptionalType)
import HOPL.INFERRED.TypeEnv
import HOPL.INFERRED.Unifier
import HOPL.Types
import Prelude hiding (exp)

data Answer = Answer {getType :: Type, getSubst :: Substitution} deriving Show

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith τ src = case result of
  Right prog -> typeOfProgram prog τ `seq` result
  _ -> result
  where
    result = parseToplevel src

reportUnequalTypes :: Type -> Type -> Exp -> Answer
reportUnequalTypes t₁ t₂ exp =
  error $
    "Types didn't match: "
      ++ show t₁
      ++ " /= "
      ++ show t₂
      ++ " in "
      ++ (show . show) exp

typeOfProgram :: Pgm -> TypeEnvironment -> Type
typeOfProgram (Pgm exp) τ = σ `seq` applySubstToType t σ
  where
    Answer t σ = typeOf exp τ emptySubst

typeOf :: Exp -> TypeEnvironment -> Substitution -> Answer
typeOf (ConstExp _) _ σ = Answer IntType σ
typeOf (VarExp x) τ σ = Answer (applyTenv τ x) σ
typeOf exp@(IsZeroExp exp₁) τ σ = Answer BoolType (unifier t IntType σ₁ exp)
  where
    Answer t σ₁ = typeOf exp₁ τ σ
typeOf (DiffExp exp₁ exp₂) τ σ = Answer IntType (unifier t₂ IntType σ₂ exp₂)
  where
    Answer t₁ σ₁ = typeOf exp₁ τ σ
    Answer t₂ σ₂ = typeOf exp₂ τ (unifier t₁ IntType σ₁ exp₁)
typeOf (LetExp x rhs body) τ σ = typeOf body τ' σ'
  where
    τ' = extendTenv x t τ
    Answer t σ' = typeOf rhs τ σ
typeOf exp@(IfExp exp₁ exp₂ exp₃) τ σ = Answer t₂ (unifier t₂ t₃ σ₃ exp)
  where
    Answer t₁ σ₁ = typeOf exp₁ τ σ
    Answer t₂ σ₂ = typeOf exp₂ τ (unifier t₁ BoolType σ₁ exp₁)
    Answer t₃ σ₃ = typeOf exp₃ τ σ₂
typeOf (ProcExp param opt body) τ σ = Answer (ProcType targ tres) σ₂
  where
    Answer targ σ₁ = optToType opt σ
    Answer tres σ₂ = typeOf body (extendTenv param targ τ) σ₁
typeOf exp@(CallExp rator rand) τ σ = Answer tres σ₄
  where
    σ₄ = unifier trator (ProcType targ tres) σ₃ exp
    Answer targ σ₃ = typeOf rand τ σ₂
    Answer trator σ₂ = typeOf rator τ σ₁
    Answer tres σ₁ = freshTvarType σ
typeOf (LetrecExp optres pname param optarg pbody body) τ σ = typeOf body τ' σ₄
  where
    Answer tres σ₁ = optToType optres σ
    Answer targ σ₂ = optToType optarg σ₁
    τ' = extendTenv pname (ProcType targ tres) τ
    Answer tpbody σ₃ = typeOf pbody (extendTenv param targ τ') σ₂
    σ₄ = unifier tpbody tres σ₃ pbody

optToType :: OptionalType -> Substitution -> Answer
optToType NoType σ = freshTvarType σ
optToType (AType t) σ = Answer t σ

freshTvarType :: Substitution -> Answer
freshTvarType σ = Answer (TypeVar $ sn σ) σ' where σ' = σ {sn = 1 + sn σ}
