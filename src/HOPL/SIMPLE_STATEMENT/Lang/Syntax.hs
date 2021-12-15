{-
 -  HOPL/SIMPLE_STATEMENT/Lang/Syntax.hs
 -
 -  Reference implementation of the toy language HOPL.SIMPLE_STATEMENT based
 -  on an exercise from the EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for the language.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.SIMPLE_STATEMENT.Lang.Syntax where

import HOPL.Types (Id)

newtype Pgm
  = Pgm Stmt
  deriving (Eq, Ord, Show)

data Stmt
  = AssignStmt Id Exp
  | PrintStmt Exp
  | MultiStmt [Stmt]
  | IfStmt Exp Stmt Stmt
  | WhileStmt Exp Stmt
  | BlockStmt [Id] Stmt
  deriving (Eq, Ord, Show)

data Exp
  = VarExp Id
  | ConstExp Integer
  | StrExp String
  | IsZeroExp Exp
  | NotExp Exp
  | DiffExp Exp Exp
  | SumExp Exp Exp
  | ProdExp Exp Exp
  | LetExp Id Exp Exp
  | LetrecExp Id Id Exp Exp
  | IfExp Exp Exp Exp
  | ProcExp [Id] Exp
  | CallExp Exp [Exp]
  | AssignExp Id Exp
  | BeginExp [Exp]
  | NewPairExp Exp Exp
  | LeftExp Exp
  | RightExp Exp
  | SetLeftExp Exp Exp
  | SetRightExp Exp Exp
  deriving (Eq, Ord, Show)
