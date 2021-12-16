{-
 -  HOPL/STATPY/LANG/DataStructures.hs
 -
 -  This module provides types for representing the values and other
 -  supporting data structures in STATPY.
 -
 -  Author: Brandon Alker, Christopher Fioti, and Nicholas Petrilli
 -}
module HOPL.STATPY.DataStructures
  ( ExpVal (..),
    DenVal,
    Binding,
    Environment (..),
    Procedure (..),
  )
where

import HOPL.STATPY.Lang.Syntax (Exp)
import HOPL.STATPY.Types (Id)

-- Denoted values are any expressed value
type DenVal = ExpVal

-- Expressed values may be th result of an expression.
data ExpVal
  = NumVal {expvalToNum :: Integer}
  | BoolVal {expvalToBool :: Bool}
  | StringVal {expvalToString :: String}
  | CharVal {expvalToChar :: Char}
  | DefVal {expvalToDef :: Procedure}
  | ListVal {expvalToList :: [ExpVal]}
  | FloatVal {expvaltoDef :: Float}
  deriving (Eq)

instance Show ExpVal where
  show (NumVal n) = "(NumVal " ++ show n ++ ")"
  show (BoolVal z) = "(BoolVal " ++ show z ++ ")"
  show (StringVal s) = "(StringVal " ++ show s ++ ")"
  show (CharVal c) = "(CharVal " ++ show c ++ ")"
  show (DefVal d) = "(DefVal " ++ show d ++ ")"
  show (ListVal vs) = "(ListVal " ++ show vs ++ ")"
  show (FloatVal fv) = "(FloatVal " ++ show fv ++ ")"
  

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
