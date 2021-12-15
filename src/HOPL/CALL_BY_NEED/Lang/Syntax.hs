{-
 -  HOPL/CALL_BY_NEED/Lang/Syntax.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for
 -  CALL_BY_NEED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CALL_BY_NEED.Lang.Syntax where

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
  | NewPairExp Exp Exp
  | LeftExp Exp
  | RightExp Exp
  | SetLeftExp Exp Exp
  | SetRightExp Exp Exp
  deriving (Eq, Ord, Show)
