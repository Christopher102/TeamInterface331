{-
 -  HOPL/INFERRED/Type.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a Haskell ADT for representing type information.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Type where

data Type
  = IntType
  | BoolType
  | ProcType Type Type
  | TypeVar Integer
  deriving (Eq, Ord)

data OptionalType
  = NoType
  | AType Type
  deriving (Eq, Ord, Show)

instance Show Type where
  show IntType = "int"
  show BoolType = "bool"
  show (ProcType targ tres) = "(" ++ show targ ++ " -> " ++ show tres ++ ")"
  show (TypeVar sn) = '%' : show sn
