{-
 -  STATPY.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the CHECKED interpreter.
 -
 -  Author: Matthew A Johnson
 -  Not Done Just Transfer of Checked Languaged Spec until ready
 -}
module STATPY (spec) where

import Data.Either (fromRight)
import HOPL.STATPY.Checker (checkWith, parseToplevel, typeOfProgram)
import HOPL.STATPY.DataStructures (ExpVal (..))
import HOPL.STATPY.Environment (Env (emptyEnv, extendEnv'))
import HOPL.STATPY.Interp (checkAndInterpWith, interpWith)
import HOPL.STATPY.Type (Type (..))
import HOPL.STATPY.TypeEnv
import Test.Tasty.Hspec

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [NumVal 1, CharVal 5, NumVal 10]
    emptyEnv

testTenv =
  extendTenv'
    ["i", "v", "x"]
    [CharType, CharType, CharType]
    emptyTenv

spec =
  describe "CHECKED tests" $ do
    describe "Value tests" $ do
      -- simple arithmetic
      specify "positive-const" $
        interp "11" `shouldBe` NumVal 11
      specify "negative-const" $
        interp "-33" `shouldBe` NumVal (-33)
      specify "simple-arith-1" $
        interp "-(44,33)" `shouldBe` NumVal 11
      -- nested arithmetic
      specify "nested-arith-left" $
        interp "-(-(44,33),22)" `shouldBe` NumVal (-11)
      specify "nested-arith-right" $
        interp "-(55, -(22,11))" `shouldBe` NumVal 44
      -- simple variables
      specify "test-var-1" $
        interp "x" `shouldBe` NumVal 10
      specify "test-var-2" $
        interp "-(x,1)" `shouldBe` NumVal 9
      specify "test-var-3" $
        interp "-(1,x)" `shouldBe` NumVal (-9)
      -- simple conditionals
      specify "if-true" $
        interp "if zero?(0) then 3 else 4" `shouldBe` NumVal 3
      specify "if-false" $
        -- make sure that the test and both arms get evaluated properly
        interp "if zero?(1) then 3 else 4" `shouldBe` NumVal 4
      specify "if-eval-test-true" $
        interp "if zero?(-(11,11)) then 3 else 4" `shouldBe` NumVal 3
      specify "if-eval-test-false" $
        interp "if zero?(-(11, 12)) then 3 else 4" `shouldBe` NumVal 4
      -- and make sure the other arm doesn't get evaluated
      specify "if-eval-test-true-2" $
        interp "if zero?(-(11, 11)) then 3 else foo" `shouldBe` NumVal 3
      specify "if-eval-test-false-2" $
        interp "if zero?(-(11,12)) then foo else 4" `shouldBe` NumVal 4
      -- simple let
      specify "simple-let-1" $
        interp "let x = 3 in x" `shouldBe` NumVal 3
      -- make sure the body and rhs get evaluated
      specify "eval-let-body" $
        interp "let x = 3 in -(x,1)" `shouldBe` NumVal 2
      specify "eval-let-rhs" $
        interp "let x = -(4,1) in -(x,1)" `shouldBe` NumVal 2
      -- check nested let and shadowing
      specify "simple-nested-let" $
        interp "let x = 3 in let y = 4 in -(x,y)" `shouldBe` NumVal (-1)
      specify "check-shadowing-in-body" $
        interp "let x = 3 in let x = 4 in x" `shouldBe` NumVal 4
      specify "check-shadowing-in-rhs" $
        interp "let x = 3 in let x = -(x,1) in x" `shouldBe` NumVal 2
      -- simple applications
      specify "apply-proc-in-rator-pos" $
        interp "(proc(x : int) -(x,1)  30)" `shouldBe` NumVal 29
      specify "interp-ignores-type-info-in-proc" $
        interp "(proc(x : (int -> int)) -(x,1)  30)" `shouldBe` NumVal 29
      specify "apply-simple-proc" $
        interp "let f = proc (x : int) -(x,1) in (f 30)" `shouldBe` NumVal 29
      specify "let-to-proc-1" $
        interp "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" `shouldBe` NumVal 29
      specify "nested-procs" $
        interp "((proc (x : int) proc (y : int) -(x,y)  5) 6)" `shouldBe` NumVal (-1)
      specify "nested-procs2" $
        interp "let f = proc(x : int) proc (y : int) -(x,y) in ((f -(10,5)) 6)"
          `shouldBe` NumVal (-1)
      specify "y-combinator-1" $
        interp
          "let fix =  proc (f : bool) \
          \             let d = proc (x : bool) proc (z : bool) ((f (x x)) z) \
          \             in proc (n : bool) ((f (d d)) n) \
          \in let t4m = proc (f : bool) proc(x : bool) if zero?(x) then 0 else -((f -(x,1)),-4) \
          \   in let times4 = (fix t4m) \
          \      in (times4 3)"
          `shouldBe` NumVal 12
      -- simple letrecs
      specify "simple-letrec-1" $
        interp "letrec int f(x : int) = -(x,1) in (f 33)" `shouldBe` NumVal 32
      specify "simple-letrec-2" $
        interp "letrec int f(x : int) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)" `shouldBe` NumVal 8
      specify "simple-letrec-3" $
        interp "let m = -5 in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)" `shouldBe` NumVal 20
      specify "fact-of-6" $
        interp
          "letrec (int -> int) mult(x : int) = proc (y : int) \
          \   if zero?(y) then 0 else -(((mult x) -(y, 1)), -(0, x)) \
          \in letrec int fact(x : int) = \
          \      if zero?(x) then 1 else ((mult x) (fact -(x, 1))) \
          \   in (fact 6)"
          `shouldBe` NumVal 720
      specify "HO-nested-letrecs" $
        interp
          "letrec (int -> int) even(odd : (int -> int)) = proc(x : int) \
          \   if zero?(x) then 1 else (odd -(x,1)) \
          \in letrec int odd(x : int) = \
          \      if zero?(x) then 0 else ((even odd) -(x,1)) \
          \   in (odd 13)"
          `shouldBe` NumVal 1

    describe "Type tests" $ do
      -- simple arithmetic
      specify "positive-const" $
        check "11" `shouldBe` IntType
      specify "negative-const" $
        check "-33" `shouldBe` IntType
      specify "simple-arith-1" $
        check "-(44,33)" `shouldBe` IntType
      -- nested arithmetic
      specify "nested-arith-left" $
        check "-(-(44,33),22)" `shouldBe` IntType
      specify "nested-arith-right" $
        check "-(55, -(22,11))" `shouldBe` IntType
      -- simple variables
      specify "test-var-1" $
        check "x" `shouldBe` IntType
      specify "test-var-2" $
        check "-(x,1)" `shouldBe` IntType
      specify "test-var-3" $
        check "-(1,x)" `shouldBe` IntType
      specify "zero-test-1" $
        check "zero?(-(3,2))" `shouldBe` BoolType
      specify "zero-test-2" $
        printCheck "-(2,zero?(0))" `shouldThrow` anyErrorCall
      -- simple conditionals
      specify "if-true" $
        check "if zero?(0) then 3 else 4" `shouldBe` IntType
      specify "if-false" $
        -- make sure that the test and both arms get evaluated properly
        check "if zero?(1) then 3 else 4" `shouldBe` IntType
      specify "if-eval-test-true" $
        check "if zero?(-(11,11)) then 3 else 4" `shouldBe` IntType
      specify "if-eval-test-false" $
        check "if zero?(-(11, 12)) then 3 else 4" `shouldBe` IntType
      -- make sure types of arms agree
      specify "if-compare-arms" $
        printCheck "if zero?(0) then 1 else zero?(1)" `shouldThrow` anyErrorCall
      specify "if-check-test-is-boolean" $
        printCheck "if 1 then 11 else 12" `shouldThrow` anyErrorCall
      -- simple let
      specify "simple-let-1" $
        check "let x = 3 in x" `shouldBe` IntType
      -- make sure the body and rhs get evaluated
      specify "eval-let-body" $
        check "let x = 3 in -(x,1)" `shouldBe` IntType
      specify "eval-let-rhs" $
        check "let x = -(4,1) in -(x,1)" `shouldBe` IntType
      -- check nested let and shadowing
      specify "simple-nested-let" $
        check "let x = 3 in let y = 4 in -(x,y)" `shouldBe` IntType
      specify "check-shadowing-in-body" $
        check "let x = 3 in let x = 4 in x" `shouldBe` IntType
      specify "check-shadowing-in-rhs" $
        check "let x = 3 in let x = -(x,1) in x" `shouldBe` IntType
      -- simple applications
      specify "apply-proc-in-rator-pos" $
        check "(proc(x : int) -(x,1)  30)" `shouldBe` IntType
      specify "checker-doesnt-ignore-type-info-in-proc" $
        printCheck "(proc(x : (int -> int)) -(x,1)  30)" `shouldThrow` anyErrorCall
      specify "apply-simple-proc" $
        check "let f = proc (x : int) -(x,1) in (f 30)" `shouldBe` IntType
      specify "let-to-proc-1" $
        check "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" `shouldBe` IntType
      specify "nested-procs" $
        check "((proc (x : int) proc (y : int) -(x,y)  5) 6)" `shouldBe` IntType
      specify "nested-procs2" $
        check "let f = proc(x : int) proc (y : int) -(x,y) in ((f -(10,5)) 6)"
          `shouldBe` IntType
      -- simple letrecs
      specify "simple-letrec-1" $
        check "letrec int f(x : int) = -(x,1) in (f 33)" `shouldBe` IntType
      specify "simple-letrec-2" $
        check "letrec int f(x : int) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
          `shouldBe` IntType
      specify "simple-letrec-3" $
        check
          "let m = -5 \
          \in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
          `shouldBe` IntType
      specify "double-it" $
        check
          "letrec int double (n : int) = if zero?(n) then 0 \
          \                              else -( (double -(n,1)), -2) \
          \in (double 3)"
          `shouldBe` IntType
      -- tests of expressions that produce procedures
      specify "build-a-proc-typed" $
        check "proc (x : int) -(x,1)" `shouldBe` ProcType IntType IntType
      specify "build-a-proc-typed-2" $
        check "proc (x : int) zero?(-(x,1))" `shouldBe` ProcType IntType BoolType
      specify "bind-a-proc-typed" $
        check "let f = proc (x : int) -(x,1) in (f 4)" `shouldBe` IntType
      specify "bind-a-proc-return-proc" $
        check "let f = proc (x : int) -(x,1) in f" `shouldBe` ProcType IntType IntType
      specify "type-a-ho-proc-1" $
        check "proc(f : (int -> bool)) (f 3)" `shouldBe` ProcType (ProcType IntType BoolType) BoolType
      specify "type-a-ho-proc-2" $
        printCheck "proc(f : (bool -> bool)) (f 3)" `shouldThrow` anyErrorCall
      specify "apply-a-ho-proc" $
        check "proc (x : int) proc (f : (int -> bool)) (f x)"
          `shouldBe` ProcType IntType (ProcType (ProcType IntType BoolType) BoolType)
      specify "apply-a-ho-proc-2" $
        check "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
          `shouldBe` ProcType
            IntType
            ( ProcType
                (ProcType IntType (ProcType IntType BoolType))
                (ProcType IntType BoolType)
            )
      specify "apply-a-ho-proc-3" $
        printCheck "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
          `shouldThrow` anyErrorCall
      specify "apply-curried-proc" $
        check "((proc(x : int) proc (y : int)-(x,y)  4) 3)" `shouldBe` IntType
      specify "apply-a-proc-2-typed" $
        check "(proc (x : int) -(x,1) 4)" `shouldBe` IntType
      specify "apply-a-letrec" $
        check
          "letrec int f(x : int) = -(x,1) \
          \in (f 40)"
          `shouldBe` IntType
      specify "letrec-non-shadowing" $
        check
          "(proc (x : int) \
          \    letrec bool loop(x : bool) = (loop x) \
          \    in x \
          \ 1)"
          `shouldBe` IntType
      specify "letrec-return-fact" $
        check
          "let times = proc (x : int) proc (y : int) -(x,y) % not really times\n \
          \in letrec \
          \      int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
          \   in fact"
          `shouldBe` ProcType IntType IntType
      specify "letrec-apply-fact" $
        check
          "let times = proc (x : int) proc (y : int) -(x,y)    % not really times\n \
          \in letrec \
          \      int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1))) \
          \   in (fact 4)"
          `shouldBe` IntType

    describe "Exception tests" $ do
      -- test dynamic typechecking
      specify "no-bool-to-diff-1" $
        printInterp "-(zero?(0),1)" `shouldThrow` anyException
      specify "no-bool-to-diff-2" $
        printInterp "-(1,zero?(0))" `shouldThrow` anyException
      specify "no-int-to-if" $
        printInterp "if 1 then 2 else 3" `shouldThrow` anyException

    describe "Error tests" $ do
      -- simple unbound variables
      specify "test-unbound-var-1" $
        printInterp "foo" `shouldThrow` anyErrorCall
      specify "test-unbound-var-1" $
        printInterp "-(x,foo)" `shouldThrow` anyErrorCall
  where
    interp = fromRight undefined . interpWith testEnv
    printInterp = print . interpWith testEnv
    check = fromRight undefined . (flip typeOfProgram testTenv <$>) . parseToplevel
    printCheck = print . (flip typeOfProgram testTenv <$>) . parseToplevel
