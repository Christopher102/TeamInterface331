{-
 -  HOPL/IMPLICIT_REFS/Syntax.hs
 -
 -  Reference implementation of the toy language IMPLICIT_REFS from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for IMPLICIT_REFS.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.IMPLICIT_REFS.Lang.Syntax where

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
  | AssignExp Id Exp
  | BeginExp [Exp]
  deriving (Eq, Ord, Show)
