{-
 -  HOPL/CHECKED/Syntax.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for CHECKED.
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
  = -- Variable reference
    VarExp Id
  | -- Integer literal
    ConstExp Integer
  | -- Arithmetic/numeric predicates
    IsZeroExp Exp
  | -- Arithmetic operators
    DiffExp Exp Exp
  | 
    AddExp Exp Exp
  |
    MultExp Exp Exp
  |
    DivExp Exp Exp
  |
    SqrtExp Exp
  |
    ExpoExp Exp Exp
  |
    ModExp Exp Exp
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
  | -- Variable declarations
    LetExp Id Exp
  | 
    LetrecExp Type Id Id Type Exp Exp
  | -- Control expressions
    IfExp Exp Exp Exp
  | -- For Lists
    EmptyExp
  |
    List [Exp]
  | -- Function definition
    DefExp Id Type Exp
  | -- Function call
    CallExp Exp Exp
  deriving (Eq, Ord, Show)
