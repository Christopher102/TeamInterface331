{-
 -  HOPL/LETREC/DataStructures.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides types for representing the values and other
 -  supporting data structures in LETREC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.DataStructures
  ( ExpVal (..),
    DenVal,
    Binding,
    Environment (..),
    Procedure (..),
  )
where

import HOPL.LETREC.Lang.Syntax (Exp)
import HOPL.Types (Id)

-- Denoted values are any expressed value
type DenVal = ExpVal

-- Expressed values may be th result of an expression.
data ExpVal
  = NumVal {expvalToNum :: Integer}
  | BoolVal {expvalToBool :: Bool}
  | ProcVal {expvalToProc :: Procedure}
  | ListVal {expvalToList :: [ExpVal]}
  deriving (Eq)

instance Show ExpVal where
  show (NumVal n) = "(NumVal " ++ show n ++ ")"
  show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
  show (ProcVal f) = "(ProcVal " ++ show f ++ ")"
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
