{-
 -  HOPL/INFERRED/Interp.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a representation for type substitutions.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Substitution where

import HOPL.INFERRED.Type

data Substitution = Substitution {substList :: [Subst], sn :: Integer} deriving (Show)

type Subst = (Type, Type)

emptySubst :: Substitution
emptySubst = Substitution [] 0

extendSubst :: Substitution -> Type -> Type -> Substitution
extendSubst σ x₁ t₁ = σ {substList = (x₁, t₁) : map (\(x, t) -> (x, applyOneSubst t x₁ t₁)) (substList σ)}

applyOneSubst :: Type -> Type -> Type -> Type
applyOneSubst t₀@(TypeVar _) tvar t₁
  | t₀ == tvar = t₁
  | otherwise = t₀
applyOneSubst (ProcType targ tres) tvar t₁ = ProcType targ' tres'
  where
    targ' = applyOneSubst targ tvar t₁
    tres' = applyOneSubst tres tvar t₁
applyOneSubst t₀ _ _ = t₀

applySubstToType :: Type -> Substitution -> Type
applySubstToType (ProcType targ tres) σ = ProcType targ' tres'
  where
    targ' = applySubstToType targ σ
    tres' = applySubstToType tres σ
applySubstToType t₀@(TypeVar _) σ@(Substitution ss sn)
  | null ss = t₀
  | t₀ == tvar = t₁
  | otherwise = applySubstToType t₀ σ₁
  where
    (tvar, t₁) = head ss
    σ₁ = σ {substList = tail ss}
applySubstToType t₀ _ = t₀
