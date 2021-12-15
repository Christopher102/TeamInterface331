{-
 -  HOPL/CALL_BY_NEED/Environment.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides an abstract data type for symbol-to-value mappings,
 -  based on a recursive data-structure representation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_NEED.Environment (Env (..)) where

import HOPL.CALL_BY_NEED.DataStructures (Binding, DenVal, Environment (..))
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
  applyEnv (Environment name val env) name'
    | name' == name = val
    | otherwise = applyEnv env name'
  extendEnv' [] [] = id
  extendEnv' vars vals =
    extendEnv' (tail vars) (tail vals) . extendEnv (head vars) (head vals)

{- Auxiliary functions -}

nobinding :: Id -> a
nobinding = error . ("No binding found for \"" ++) . (++ "\"")
