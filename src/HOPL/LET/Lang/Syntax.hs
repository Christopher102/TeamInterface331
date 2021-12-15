{-
 -  HOPL/LET/Syntax.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the abstract syntax representation for LET.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Lang.Syntax where

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
  deriving (Eq, Ord, Show)
