{-
 -  HOPL/CALL_BY_REFERENCE/DataStructures.hs
 -
 -  Reference implementation of the toy language CALL_BY_REFERENCE from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides types for representing the values and other
 -  supporting data structures in CALL_BY_REFERENCE.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_REFERENCE.DataStructures
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

import HOPL.CALL_BY_REFERENCE.Lang.Syntax (Exp)
import HOPL.Types (Id, Reference)

--Storable values are any expressed value
type StoVal = ExpVal

-- Denoted values are any expressed value
type DenVal = Reference

-- Mutable pairs
type MutPair = Pair2

type Pair1 = (Reference, Reference)

type Pair2 = Reference

-- Expressed values may be th result of an expression.
data ExpVal
  = NumVal {expvalToNum :: Integer}
  | BoolVal {expvalToBool :: Bool}
  | ProcVal {expvalToProc :: Procedure}
  | MutPairVal {expvalToPair :: MutPair}
  deriving (Eq)

instance Show ExpVal where
  show (NumVal n) = "(NumVal " ++ show n ++ ")"
  show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
  show (ProcVal f) = "(ProcVal " ++ show f ++ ")"
  show (MutPairVal pr) = "(MutPairVal " ++ show pr ++ ")"

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
