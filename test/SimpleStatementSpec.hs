{-
 -  SimpleStatementSpec.hs
 -
 -  Reference implementation of the toy language SIMPLE_STATEMENT based on an
 -  exercise from the EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the SIMPLE_STATEMENT interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module SimpleStatementSpec (spec) where

import Data.Either (fromRight)
import HOPL.SIMPLE_STATEMENT.DataStructures (ExpVal (..), Procedure (..))
import HOPL.SIMPLE_STATEMENT.Environment (Env (..))
import HOPL.SIMPLE_STATEMENT.Interp (interpWith)
import HOPL.SIMPLE_STATEMENT.Lang.Syntax (Exp (..))
import HOPL.SIMPLE_STATEMENT.Store (Store, emptyStore)
import Test.Tasty.Hspec

testStore = emptyStore

testEnv = emptyEnv

spec =
  describe "SIMPLE_STATEMENT tests" $ do
    describe "Run tests" $ do
      specify "Example 1" $
        interp "var x,y; { x = 3; y = 4; print +(x,y)}"
          `shouldReturn` testStore ++ [NumVal 3, NumVal 4]
      specify "Example 2" $
        interp
          "var x,y,z; { \
          \   x = 3; y = 4; z = 0; \
          \   while not(zero?(x)) { \
          \      z = +(z,y); \
          \      x = -(x,1) \
          \   }; \
          \   print z \
          \}"
          `shouldReturn` testStore ++ [NumVal 0, NumVal 4, NumVal 12]
      specify "Example 3" $
        interp
          "var x; { \
          \   x = 3; \
          \   print x; \
          \   var x; { \
          \      x = 4; \
          \      print x \
          \   }; \
          \   print x \
          \}"
          `shouldReturn` testStore ++ [NumVal 3, NumVal 4]
      specify "Example 4" $
        interp
          "var f,x; { \
          \   f = proc(x,y) *(x,y); \
          \   x = 3; \
          \   print (f 4 x) \
          \}"
          `shouldReturn` testStore
            ++ [ ProcVal $
                   ClosedProcedure
                     { procVars = ["x", "y"],
                       procBody = ProdExp (VarExp "x") (VarExp "y"),
                       procEnv = extendEnv' ["f", "x"] [0, 1] emptyEnv
                     },
                 NumVal 3,
                 NumVal 3,
                 NumVal 4
               ]
    describe "Exception tests" $ do
      specify "Uninitialized variable" $
        interp "var x; print x" `shouldThrow` anyException
  where
    interp = fromRight undefined . interpWith testEnv testStore
