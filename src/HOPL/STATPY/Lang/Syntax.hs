{-
 -  HOPL/STATPY/LANG/Syntax.hs
 -
 -  This module provides the abstract syntax representation for STATPY.
 -
 -  Authors: Brandon Alker, Nick Petrilli, and Christopher Fioti
 -}
module HOPL.STATPY.Lang.Syntax where

import HOPL.STATPY.Type (Type)
import HOPL.STATPY.Types (Id)

newtype Pgm
  = Pgm Exp
  deriving (Eq, Ord, Show)

-- For each non-terminal appearing on the right-hand side of a production
-- we include a parameter type for the corresponding data constructor.
data Exp
  = 
    -- Control expressions
    IfExp Exp Exp Exp
  | -- Function definition
    DefExp Type Id Exp
  | -- Function call
    CallExp Exp Exp
  | -- Arithmetic operators
    DiffExp Exp Exp
  | 
    AddExp Exp Exp
  |
    MultExp Exp Exp
  |
    DivExp Exp Exp
  |
    ExpoExp Exp Exp
  |
    ModExp Exp Exp
  |
    SqrtExp Exp
  |
    GreaterExp Exp Exp 
  |
    LessExp Exp Exp
  |
    GreatEqExp Exp Exp
  |
    LessEqExp Exp Exp
  |
    EqualExp Exp Exp
  |
    NotEqualExp Exp Exp
  |
    NotExp Exp
  | 
    IsZeroExp Exp
  | -- For Lists
    EmptyListExp Type
  |
    ListExp [Exp]
  | -- Integer literal
    ConstExp Integer
  |  -- Variable reference
    VarExp Id
  deriving (Eq, Ord, Show)
