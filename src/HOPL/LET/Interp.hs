{-
 -  HOPL/LET/Interp.hs
 -
 -  Reference implementation of the toy language HOPL.LET by Mitchell Wand.
 -  This module provides the core interpreter implementation.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.LET.Interp
  ( interp,
    interpWith,
    interpWith',
  )
where

import Data.Either (fromRight)
import HOPL.LET.DataStructures (DenVal, Environment, ExpVal (..))
import HOPL.LET.Environment (Env (..))
import HOPL.LET.Lang.Parser (ParseError, parseToplevel)
import HOPL.LET.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.Types (Source)
import Prelude hiding (exp)

{- top-level interpreter routines -}

interp :: Source -> Either ParseError ExpVal
interp = interpWith emptyEnv

interpWith' :: Environment -> Source -> ExpVal
interpWith' ρ = fromRight undefined . interpWith ρ

interpWith :: Environment -> Source -> Either ParseError ExpVal
interpWith ρ src = flip valueOfProgram ρ <$> parseToplevel src

{- semantic reduction of a program -}

valueOfProgram :: Pgm -> Environment -> ExpVal
valueOfProgram (Pgm exp) ρ = valueOf exp ρ

{- semantic reductions for expressions -}

valueOf :: Exp -> Environment -> ExpVal
valueOf (ConstExp n) _ = NumVal n
valueOf (VarExp x) ρ = applyEnv ρ x
valueOf (IsZeroExp exp₁) ρ = BoolVal (n == 0)
  where
    NumVal n = valueOf exp₁ ρ
valueOf (DiffExp exp₁ exp₂) ρ = NumVal (n₁ - n₂)
  where
    NumVal n₁ = valueOf exp₁ ρ
    NumVal n₂ = valueOf exp₂ ρ
valueOf (LetExp x rhs body) ρ = valueOf body ρ'
  where
    ρ' = extendEnv x v ρ
    v = valueOf rhs ρ
valueOf (IfExp exp₁ exp₂ exp₃) ρ = valueOf exp' ρ
  where
    exp' = case valueOf exp₁ ρ of
      BoolVal True -> exp₂
      BoolVal False -> exp₃
