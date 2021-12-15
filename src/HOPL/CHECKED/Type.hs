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
module HOPL.CHECKED.Type where

data Type
  = IntType
  | BoolType
  | ProcType Type Type
  deriving (Eq, Ord)

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show (ProcType targ tres) = "(" ++ show targ ++ " -> " ++ show tres ++ ")"
