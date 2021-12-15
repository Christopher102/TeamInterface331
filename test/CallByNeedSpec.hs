{-
 -  CallByNeedSpec.hs
 -
 -  Reference implementation of the toy language CALL_BY_NEED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides a test suite for the CALL_BY_NEED interpreter.
 -
 -  Author: Matthew A Johnson
 -}
module CallByNeedSpec (spec) where

import Data.Either (fromRight)
import HOPL.CALL_BY_NEED.DataStructures (ExpVal (..), StoVal (StoVal))
import HOPL.CALL_BY_NEED.Environment (Env (emptyEnv, extendEnv'))
import HOPL.CALL_BY_NEED.Interp (interpWith)
import HOPL.CALL_BY_NEED.Store (emptyStore)
import Test.Tasty.Hspec

testStore = map StoVal [NumVal 1, NumVal 5, NumVal 10]

testEnv =
  extendEnv'
    ["i", "v", "x"]
    [0, 1, 2]
    emptyEnv

spec =
  describe "CALL_BY_REFERENCE tests" $ do
    describe "Value tests" $ do
      specify "positive-const" $
        interp "11" `shouldBe` NumVal 11
      specify "negative-const" $
        interp "-33" `shouldBe` NumVal (-33)
      specify "simple-arith-1" $
        interp "-(44,33)" `shouldBe` NumVal 11
      specify "nested-arith-left" $
        interp "-(-(44,33),22)" `shouldBe` NumVal (-11)
      specify "nested-arith-right" $
        interp "-(55, -(22,11))" `shouldBe` NumVal 44
      specify "test-var-1" $
        interp "x" `shouldBe` NumVal 10
      specify "test-var-2" $
        interp "-(x,1)" `shouldBe` NumVal 9
      specify "test-var-3" $
        interp "-(1,x)" `shouldBe` NumVal (-9)
      specify "if-true" $
        interp "if zero?(0) then 3 else 4" `shouldBe` NumVal 3
      specify "if-false" $
        interp "if zero?(1) then 3 else 4" `shouldBe` NumVal 4
      specify "if-eval-test-true" $
        interp "if zero?(-(11,11)) then 3 else 4" `shouldBe` NumVal 3
      specify "if-eval-test-false" $
        interp "if zero?(-(11, 12)) then 3 else 4" `shouldBe` NumVal 4
      specify "if-eval-test-true-2" $
        interp "if zero?(-(11, 11)) then 3 else foo" `shouldBe` NumVal 3
      specify "if-eval-test-false-2" $
        interp "if zero?(-(11,12)) then foo else 4" `shouldBe` NumVal 4
      specify "simple-let-1" $
        interp "let x = 3 in x" `shouldBe` NumVal 3
      specify "eval-let-body" $
        interp "let x = 3 in -(x,1)" `shouldBe` NumVal 2
      specify "eval-let-rhs" $
        interp "let x = -(4,1) in -(x,1)" `shouldBe` NumVal 2
      specify "simple-nested-let" $
        interp "let x = 3 in let y = 4 in -(x,y)" `shouldBe` NumVal (-1)
      specify "check-shadowing-in-body" $
        interp "let x = 3 in let x = 4 in x" `shouldBe` NumVal 4
      specify "check-shadowing-in-rhs" $
        interp "let x = 3 in let x = -(x,1) in x" `shouldBe` NumVal 2
      specify "apply-proc-in-rator-pos" $
        interp "(proc(x) -(x,1)  30)" `shouldBe` NumVal 29
      specify "apply-simple-proc" $
        interp "let f = proc (x) -(x,1) in (f 30)" `shouldBe` NumVal 29
      specify "let-to-proc-1" $
        interp "(proc(f)(f 30)  proc(x)-(x,1))" `shouldBe` NumVal 29
      specify "nested-procs" $
        interp "((proc (x) proc (y) -(x,y)  5) 6)" `shouldBe` NumVal (-1)
      specify "nested-procs2" $
        interp "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
          `shouldBe` NumVal (-1)
      specify "y-combinator-1" $
        interp
          "let fix = proc (f) \
          \             let d = proc (x) proc (z) ((f (x x)) z) \
          \             in proc (n) ((f (d d)) n) \
          \in let t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4) \
          \   in let times4 = (fix t4m) \
          \      in (times4 3)"
          `shouldBe` NumVal 12
      specify "simple-letrec-1" $
        interp "letrec f(x) = -(x,1) in (f 33)" `shouldBe` NumVal 32
      specify "simple-letrec-2" $
        interp "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)" `shouldBe` NumVal 8
      specify "simple-letrec-3" $
        interp "let m = -5 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)" `shouldBe` NumVal 20
      specify "HO-nested-letrecs" $
        interp
          "letrec even(odd) = proc(x) if zero?(x) then 1 else (odd -(x,1)) \
          \in letrec odd(x) = if zero?(x) then 0 else ((even odd) -(x,1)) \
          \   in (odd 13)"
          `shouldBe` NumVal 1
      specify "begin-test-1" $
        interp "begin 1; 2; 3 end" `shouldBe` NumVal 3
      specify "gensym-test" $
        interp
          "let g = let count = 0 \
          \        in proc(d) let d = set count = -(count,-1) \
          \                   in count \
          \in -((g 11), (g 22))"
          `shouldBe` NumVal (-1)
      specify "assignment-test-1" $
        interp
          "let x = 17 \
          \in begin set x = 27; x end"
          `shouldBe` NumVal 27
      -- specify "even-odd-via-set" $
      --   interp "let x = 0 \
      --          \in letrec even(d) = if zero?(x) then 1 \
      --                              \else let d = set x = -(x,1) \
      --                                   \in (odd d) \
      --                    \odd(d)  = if zero?(x) then 0 \
      --                              \else let d = set x = -(x,1) \
      --                                   \in (even d) \
      --             \in let d = set x = 13 in (odd -99)" `shouldBe` NumVal 1
      specify "example-for-book-1" $
        interp
          "let f = proc (x) proc (y) \
          \                    begin \
          \                       set x = -(x,-1); \
          \                       -(x,y) \
          \                    end \
          \in ((f 44) 33)"
          `shouldBe` NumVal 12
      specify "simple-mutpair-left-1" $
        interp "let p = newpair(22,33) in left(p)" `shouldBe` NumVal 22
      specify "simple-mutpair-right-1" $
        interp "let p = newpair(22,33) in right(p)" `shouldBe` NumVal 33
      specify "simple-mutpair-setleft-1" $
        interp
          "let p = newpair(22,33) \
          \in begin setleft p = 77; \
          \      left(p) \
          \   end"
          `shouldBe` NumVal 77
      specify "simple-mutpair-setleft-2" $
        interp
          "let p = newpair(22,33) \
          \in begin setleft p = 77; \
          \      right(p) \
          \   end"
          `shouldBe` NumVal 33
      specify "simple-mutpair-setright-1" $
        interp
          "let p = newpair(22,33) \
          \in begin setright p = 77; \
          \      right(p) \
          \   end"
          `shouldBe` NumVal 77
      specify "simple-mutpair-setright-2" $
        interp
          "let p = newpair(22,33) \
          \in begin setright p = 77; \
          \      left(p) \
          \   end"
          `shouldBe` NumVal 22
      specify "gensym-using-mutable-pair-left" $
        interp
          "let g = let count = newpair(0,0) \
          \        in proc (dummy) \
          \              begin \
          \                 setleft count = -(left(count), -1); \
          \                 left(count) \
          \              end \
          \in -((g 22), (g 22))"
          `shouldBe` NumVal (-1)
      specify "gensym-using-mutable-pair-right" $
        interp
          "let g = let count = newpair(0,0) \
          \        in proc (dummy) \
          \              begin \
          \                 setright count = -(right(count), -1); \
          \                 right(count) \
          \              end \
          \in -((g 22), (g 22))"
          `shouldBe` NumVal (-1)
      specify "example-for-mutable-pairs-section" $
        interp
          "let glo = newpair(11,22) \
          \in let f = proc (loc) \
          \              begin  % this is a comment\n \
          \                 setright loc = left(loc); \
          \                 setleft  glo = 99; \
          \                 -(left(loc),right(loc)) \
          \              end \
          \   in (f glo)"
          `shouldBe` NumVal 88

      -- new for call-by-reference
      specify "cbr-swap-1" $
        interp
          "let swap = proc (x) proc (y) \
          \              let temp = x \
          \              in begin \
          \                    set x = y; \
          \                    set y = temp \
          \                 end \
          \in let a = 33 \
          \   in let b = 44 \
          \      in begin \
          \            ((swap a) b); \
          \            -(a,b) \
          \         end"
          `shouldBe` NumVal 11
      specify "cbr-global-aliasing-1" $
        interp
          "let p = proc (z) set z = 44 \
          \in let x = 33 \
          \   in begin (p x); x end"
          `shouldBe` NumVal 44
      specify "cbr-direct-aliasing-1" $
        interp
          "let p = proc (x) proc (y) \
          \           begin \
          \              set x = 44; \
          \              y \
          \           end \
          \in let b = 33 \
          \   in ((p b) b)"
        `shouldBe` NumVal 44
      specify "cbn-indirect-aliasing-1" $
        -- in this language, you can't return a reference.
        interp
          "let p = proc (x) proc (y) \
          \           begin \
          \              set x = 44; \
          \              y \
          \           end \
          \in let q = proc(z) z \
          \   in let b = 33 \
          \      in ((p b) (q b))"
          `shouldBe` NumVal 44
      specify "cbn-indirect-aliasing-2" $
        -- in this language, you can't return a reference.
        interp
          "let p = proc (x) proc (y) \
          \           begin \
          \              set x = 44; \
          \              y \
          \           end \
          \in let q = proc(z) z \
          \   in let b = 33 \
          \      in ((p (q b)) b)"
          `shouldBe` NumVal 33
      specify "cbr-sideeffect-a-passed-structure-1" $
        interp
          "let f = proc (x) setleft x = -(left(x),-1) \
          \in let p = newpair (44,newpair(55,66)) \
          \   in begin \
          \         (f right(p)); \
          \         left(right(p)) \
          \      end"
          `shouldBe` NumVal 56
      specify "cbr-example-for-book" $
        interp
          "let f = proc (x) set x = 44 \
          \in let g = proc (y) (f y) \
          \   in let z = 55 \
          \      in begin \
          \            (g z); \
          \            z \
          \         end"
          `shouldBe` NumVal 44
      specify "cbn-multiply" $
        interp
          "let makerec = proc (f) \
          \                 let d = proc (x) (f (x x)) \
          \                 in (f (d d)) \
          \in let maketimes4 = proc (f) \
          \                       proc (x) \
          \                          if zero?(x) \
          \                          then 0 \
          \                          else -((f -(x,1)), -4) \
          \   in let times4 = (makerec maketimes4) \
          \      in (times4 3)"
          `shouldBe` NumVal 12
      specify "cbn-avoid-nontermination" $
        interp
          "letrec infinite-loop (x) = (infinite-loop -(x,-1)) \
          \in let f = proc (z) 11 \
          \   in (f (infinite-loop 0))"
          `shouldBe` NumVal 11

    describe "Exception tests" $ do
      specify "no-bool-to-diff-1" $
        printInterp "-(zero?(0),1)" `shouldThrow` anyException
      specify "no-bool-to-diff-2" $
        printInterp "-(1,zero?(0))" `shouldThrow` anyException
      specify "no-int-to-if" $
        printInterp "if 1 then 2 else 3" `shouldThrow` anyException

    describe "Error tests" $ do
      specify "test-unbound-var-1" $
        printInterp "foo" `shouldThrow` anyErrorCall
      specify "test-unbound-var-1" $
        printInterp "-(x,foo)" `shouldThrow` anyErrorCall
  where
    interp = fromRight undefined . interpWith testEnv testStore
    printInterp = print . interpWith testEnv testStore
