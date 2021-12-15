{-
 -  HOPL/CHECKED_STATEMENT/Syntax.hs
 -
 -  Reference implementation of the toy language CHECKED_STATEMENT from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the abstract syntax representation for CHECKED_STATEMENT.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.CHECKED_STATEMENT.Lang.Syntax where

import HOPL.CHECKED_STATEMENT.Type (Type)
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
  | BlockStmt [Id] [Type] Stmt
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
  | LetrecExp Type Id Id Type Exp Exp
  | IfExp Exp Exp Exp
  | ProcExp Id Type Exp
  | CallExp Exp Exp
  | AssignExp Id Exp
  | BeginExp [Exp]
  | NewPairExp Exp Exp
  | UnpairExp Id Id Exp Exp
  | SetLeftExp Exp Exp
  | SetRightExp Exp Exp
  --  | LeftExp Exp
  --  | RightExp Exp
  deriving (Eq, Ord, Show)
