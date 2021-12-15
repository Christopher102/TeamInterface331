{-
 -  HOPL/Types.hs
 -
 -  Reference implementation of the languages from EOPL3 by Mitchell Wand.
 -
 -  This module provides type definitions used throughout other sources.
 -
 -  Author: Matthew A Johnson
 -}
module HOPL.Types where

import Text.ParserCombinators.Parsec.Error (ParseError)

-- Represent identifiers using a Haskell String
type Id = String

-- Source code is simply a text string
type Source = String

-- References are just integer indexes into the store
type Reference = Int

-- Interpreter takes source code and produces a value (of some type)
type Interpreter a = Source -> Either ParseError a
