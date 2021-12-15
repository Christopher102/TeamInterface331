{-
 -  HOPL/CHECKED_STATEMENT/DataStructures.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides types for representing the values and other
 -  supporting data structures in CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED_STATEMENT.DataStructures
  ( ExpVal (..),
    DenVal,
    StoVal,
    MutPair,
    Pair1,
    Pair2,
    Binding,
    Environment (..),
    Procedure (..),
  )
where

import HOPL.CHECKED_STATEMENT.Lang.Syntax (Exp)
import HOPL.Types (Id, Reference)

--Storable values are any expressed value
type StoVal = ExpVal

-- Denoted values are any expressed value
type DenVal = Reference

-- Mutable pairs
type MutPair = Pair1

type Pair1 = (Reference, Reference)

type Pair2 = Reference

-- Expressed values may be th result of an expression.
data ExpVal
  = NumVal {expvalToNum :: Integer}
  | BoolVal {expvalToBool :: Bool}
  | StrVal {expvalToString :: String}
  | ProcVal {expvalToProc :: Procedure}
  | MutPairVal {expvalToPair :: MutPair}
  | ListVal {expvalToList :: [ExpVal]}
  deriving (Eq)

instance Show ExpVal where
  show (NumVal n) = show n
  show (BoolVal z) = show z
  show (StrVal s) = s
  show (ProcVal f) = show f
  show (MutPairVal pr) = show pr
  show (ListVal vs) = "(ListVal " ++ show vs ++ ")"

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

{- Representation of closed procedure (i.e. closure) -}

data Procedure
  = ClosedProcedure {procVar :: Id, procBody :: Exp, procEnv :: Environment}
  | OpenProcedure {procVar :: Id, procBody :: Exp}
  deriving (Eq, Show)
