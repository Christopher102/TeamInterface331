{-
 -  HOPL/STATPY/Environment.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides an abstract data type for symbol-to-type mappings,
 -  based on a recursive data-structure representation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.STATPY.TypeEnv (TEnv (..), TypeEnvironment, Type (..)) where

import Data.List
import Data.Maybe
import HOPL.STATPY.DataStructures
import HOPL.STATPY.Type
import HOPL.STATPY.Types (Id)

nobinding = error . ("No binding found for \"" ++) . (++ "\"")

type Var = String

type Binding t = (Var, t)

{- interface for an environment (symbol-to-value mapping) -}
class TEnv e where
  -- construct an tempty environment
  emptyTenv :: e

  -- extracts form an environment the mapped value if search symbol is present
  applyTenv :: e -> Id -> Type

  -- construct new environment from existing environment plus a new binding
  extendTenv :: Id -> Type -> e -> e

  -- construct new environment from existing environment plus new bindings
  extendTenv' :: [Id] -> [Type] -> e -> e
  extendTenv' (x : xs) (t : ts) e = extendTenv' xs ts (extendTenv x t e)
  extendTenv' [] [] e = e

{- Recursive "data structure" representation for environments -}
data TypeEnvironment
  = EmptyTypeEnv
  | ExtendedTypeEnv Id Type TypeEnvironment

instance TEnv TypeEnvironment where
  emptyTenv = EmptyTypeEnv
  extendTenv = ExtendedTypeEnv
  applyTenv EmptyTypeEnv x = nobinding x
  applyTenv e'@(ExtendedTypeEnv x t e) y
    | x == y = t
    | otherwise = applyTenv e y
