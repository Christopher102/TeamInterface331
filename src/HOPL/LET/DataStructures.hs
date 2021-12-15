{-
 -  HOPL/LET/DataStructures.hs
 -
 -  Reference implementation of the toy language LET by Mitchell Wand.
 -  This module provides types for representing the values and other
 -  supporting data structures in LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.DataStructures
  ( ExpVal (..),
    DenVal,
    Binding,
    Environment (..),
  )
where

import HOPL.LET.Lang.Syntax (Exp)
import HOPL.Types (Id)

-- Denoted values are any expressed value
type DenVal = ExpVal

-- Expressed values may be th result of an expression.
data ExpVal
  = NumVal {expvalToNum :: Integer}
  | BoolVal {expvalToBool :: Bool}
  deriving (Eq)

instance Show ExpVal where
  show (NumVal n) = "(NumVal " ++ show n ++ ")"
  show (BoolVal z) = "(BoolVal " ++ show z ++ ")"

{- Recursive "data structure" representation for environments -}

type Binding = (Id, DenVal)

data Environment = EmptyEnvironment | Environment Id DenVal Environment
  deriving (Eq)

instance Show Environment where
  show env = show $ envToList env

{- Auxiliary functions -}

envToList :: Environment -> [Binding]
envToList EmptyEnvironment = []
envToList (Environment x v savedEnv) = (x, v) : envToList savedEnv
