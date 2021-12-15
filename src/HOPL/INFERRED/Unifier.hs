{-
 -  HOPL/INFERRED/Interp.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides an implementation for the type unifier.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Unifier where

import Prelude hiding (exp)

import HOPL.INFERRED.Lang.Syntax
import HOPL.INFERRED.Substitution
import HOPL.INFERRED.Type

unifier :: Type -> Type -> Substitution -> Exp -> Substitution
unifier t₁ t₂ σ exp 
  | t₁' == t₂' = σ
  | otherwise = unifier' t₁' t₂' σ exp
  where
    t₁' = applySubstToType t₁ σ
    t₂' = applySubstToType t₂ σ

unifier' :: Type -> Type -> Substitution -> Exp -> Substitution
unifier' t₁@(TypeVar _) t₂ σ exp
    | hasNoOccurrence t₁ t₂ = extendSubst σ t₁ t₂
    | otherwise = reportNoOccurrenceViolation t₁ t₂ exp
unifier' t₁ t₂@(TypeVar _) σ exp
    | hasNoOccurrence t₂ t₁ = extendSubst σ t₂ t₁
    | otherwise = reportNoOccurrenceViolation t₂ t₁ exp
unifier' (ProcType targ₁ tres₁) (ProcType targ₂ tres₂) σ exp = σ₂
  where
    σ₂ = unifier tres₁ tres₂ σ₁ exp
    σ₁ = unifier targ₁ targ₂ σ  exp
unifier' t₁ t₂ _ exp = reportUnificationFailure t₁ t₂ exp

reportUnificationFailure :: Type -> Type -> Exp -> a
reportUnificationFailure t₁ t₂ exp
    = error $ "Type mismatch: " ++ show t₁ ++ " doesn't match " ++ show t₂
        ++ " in " ++ show exp

reportNoOccurrenceViolation :: Type -> Type -> Exp -> a
reportNoOccurrenceViolation t₁ t₂ exp
    = error $ "Can't unify: type variable " ++ show t₁ ++ " occurs in type " ++ show t₂
        ++ " in expression " ++ show exp

hasNoOccurrence :: Type -> Type -> Bool
hasNoOccurrence tvar (ProcType targ tres) = hasNoOccurrence tvar targ && hasNoOccurrence tvar tres
hasNoOccurrence tvar t₂@(TypeVar _) = tvar/= t₂
hasNoOccurrence _ _ = True
