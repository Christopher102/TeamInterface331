{-
 -  HOPL/STATPY/Environment.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides an abstract data type for symbol-to-value mappings,
 -  based on a recursive data-structure representation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.STATPY.Environment (Env (..)) where

import HOPL.STATPY.DataStructures (Binding, DenVal, Environment (..), ExpVal (DefVal), Procedure (..))
import HOPL.Types (Id)

{- Interface for an environment (symbol-to-value mapping) -}

class Env e where
  -- construct an emptyEnv environment
  emptyEnv :: e

  -- construct new environment from existing environment plus a new binding
  extendEnv :: Id -> DenVal -> e -> e

  -- extract from an environment the mapped value if search symbol is present
  applyEnv :: e -> Id -> DenVal

  -- construct new environment from existing environment plus new bindings
  extendEnv' :: [Id] -> [DenVal] -> e -> e

{- Implementation of environment interface using data structure representation -}

instance Env Environment where
  emptyEnv = EmptyEnvironment
  extendEnv = Environment
  applyEnv EmptyEnvironment name = nobinding name
  applyEnv ρ@(Environment name val ρ') name'
    | name' == name = expand ρ val
    | otherwise = applyEnv ρ' name'
    where
      expand ρ (DefVal (OpenProcedure param body)) = DefVal (ClosedProcedure param body ρ)
      expand _ v = v

  extendEnv' [] [] = id
  extendEnv' vars vals =
    extendEnv' (tail vars) (tail vals) . extendEnv (head vars) (head vals)

{- Auxiliary functions -}

nobinding :: Id -> a
nobinding = error . ("No binding found for \"" ++) . (++ "\"")
