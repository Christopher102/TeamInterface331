{-
 -  HOPL/EXPLICIT_REFS/Syntax.hs
 -
 -  Reference implementation of the toy language EXPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for EXPLICIT_REFS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.EXPLICIT_REFS.Lang.Syntax where

import HOPL.Types (Id)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

data Exp
  = VarExp Id
  | ConstExp Integer
  | IsZeroExp Exp
  | DiffExp Exp Exp
  | LetExp Id Exp Exp
  | LetrecExp Id Id Exp Exp
  | IfExp Exp Exp Exp
  | ProcExp Id Exp
  | CallExp Exp Exp
  | NewrefExp Exp
  | DerefExp  Exp
  | SetrefExp Exp Exp
  | BeginExp [Exp]
  deriving (Eq, Ord, Show)
