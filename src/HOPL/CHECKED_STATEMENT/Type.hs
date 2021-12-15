{-
 -  HOPL/CHECKED/Type.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a Haskell ADT for representing type information.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED_STATEMENT.Type where

data Type
  = IntType
  | BoolType
  | StrType
  | ProcType Type Type
  | PairType Type Type
  deriving (Eq, Ord)

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show StrType = "str"
  show (ProcType targ tres) = "(" ++ show targ ++ " -> " ++ show tres ++ ")"
  show (PairType tleft tright) = "pairof(" ++ show tleft ++ " * " ++ show tright ++ ")"
