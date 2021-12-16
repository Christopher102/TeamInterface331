{-
 -  HOPL/STATPY/Type.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a Haskell ADT for representing type information.
 -
 -  Author: Brandon Alker, Christopher Fioti, and Nicholas Petrilli
 -}
module HOPL.STATPY.Type where

data Type
  = IntType
  | BoolType
  | StringType
  | CharType
  | ListType
  | FloatType
  | DefType Type Type
  deriving (Eq, Ord)

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show StringType = "string"
  show CharType = "char"
  show ListType = "list"
  show FloatType = "float"
  show (DefType targ tres) = "(" ++ show targ ++ " -> " ++ show tres ++ ")"
  
  
