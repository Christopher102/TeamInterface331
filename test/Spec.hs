{-
 -  Spec.hs
 -
 -  This module runs test suites for all HOPL language implementations.
 -
 -  Author: Matthew A Johnson
 -}
module Main where

import qualified CallByNeedSpec as CALL_BY_NEED
import qualified CallByReferenceSpec as CALL_BY_REFERENCE
import qualified CheckedSpec as CHECKED
import qualified ExplicitRefsSpec as EXPLICIT_REFS
import qualified ImplicitRefsSpec as IMPLICIT_REFS
import qualified InferredSpec as INFERRED
import qualified LetSpec as LET
import qualified LetrecSpec as LETREC
import qualified MutablePairsSpec as MUTABLE_PAIRS
import qualified ProcSpec as PROC
import qualified SimpleStatementSpec as SIMPLE_STATEMENT
import qualified STATPY as STATPY
import Test.Tasty.Hspec (hspec)

main = do
  hspec LET.spec
  hspec PROC.spec
  hspec LETREC.spec
  hspec EXPLICIT_REFS.spec
  hspec IMPLICIT_REFS.spec
  hspec MUTABLE_PAIRS.spec
  hspec CALL_BY_REFERENCE.spec
  hspec CALL_BY_NEED.spec
  hspec CHECKED.spec
  hspec INFERRED.spec
  hspec SIMPLE_STATEMENT.spec
  hspec STATPY.spec
