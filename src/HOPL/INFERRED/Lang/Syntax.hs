{-
 -  HOPL/INFERRED/Syntax.hs
 -
 -  Reference implementation of the toy language INFERRED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for INFERRED.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.INFERRED.Lang.Syntax where

import HOPL.INFERRED.Type
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
  | LetrecExp OptionalType Id Id OptionalType Exp Exp
  | -- Control expressions
    IfExp Exp Exp Exp
  | -- Function definition
    ProcExp Id OptionalType Exp
  | -- Function call
    CallExp Exp Exp
  deriving (Eq, Ord, Show)
