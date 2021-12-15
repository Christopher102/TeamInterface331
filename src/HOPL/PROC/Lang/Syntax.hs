{-
 -  HOPL/PROC/Syntax.hs
 -
 -  Reference implementation of the toy language PROC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for PROC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.PROC.Lang.Syntax where

import HOPL.Types (Id)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

data Exp
  = ConstExp Integer
  | VarExp Id
  | IsZeroExp Exp
  | DiffExp Exp Exp
  | LetExp Id Exp Exp
  | IfExp Exp Exp Exp
  | ProcExp Id Exp
  | CallExp Exp Exp
  deriving (Eq, Ord, Show)
