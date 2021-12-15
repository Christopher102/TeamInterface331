{-
 -  HOPL/CHECKED/Syntax.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED.Lang.Syntax where

import HOPL.CHECKED.Type (Type)
import HOPL.Types (Id)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

-- For each non-terminal appearing on the right-hand side of a production
-- we include a parameter type for the corresponding data constructor.
data Exp
  = -- Variable reference
    VarExp Id
  | -- Integer literal
    ConstExp Integer
  | -- Arithmetic/numeric predicates
    IsZeroExp Exp
  | -- Arithmetic operators
    DiffExp Exp Exp
  | -- Variable declarations
    LetExp Id Exp Exp
  | LetrecExp Type Id Id Type Exp Exp
  | -- Control expressions
    IfExp Exp Exp Exp
  | -- Function definition
    ProcExp Id Type Exp
  | -- Function call
    CallExp Exp Exp
  deriving (Eq, Ord, Show)
