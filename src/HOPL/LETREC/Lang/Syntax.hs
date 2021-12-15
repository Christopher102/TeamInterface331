{-
 -  HOPL/LETREC/Syntax.hs
 -
 -  Reference implementation of the toy language LETREC from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for LETREC.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LETREC.Lang.Syntax where

import GHC.TypeNats (Mod)
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
  | -- Boolean literals
    TrueExp
  | FalseExp
  | -- Arithmetic operators
    UnaryExp UnaryOp Exp
  | BinaryExp BinaryOp Exp Exp
  | -- List constructors
    EmptyExp
  | ListExp [Exp]
  | -- Variable declarations
    LetExp Id Exp Exp
  | LetrecExp Id Id Exp Exp
  | UnpackExp [Id] Exp Exp
  | -- Control expressions
    IfExp Exp Exp Exp
  | -- Function definition
    ProcExp Id Exp
  | -- Function call
    CallExp Exp Exp
  deriving (Eq, Ord, Show)

data UnaryOp
  = IsZero
  | IsNeg
  | IsPos
  | Minus
  | Not
  | IsNull
  | Car
  | Cdr
  deriving (Eq, Ord, Show)

data BinaryOp
  = Diff
  | Plus
  | Times
  | Divides
  | Mod
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | And
  | Or
  | Cons
  deriving (Eq, Ord, Show)
