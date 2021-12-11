module HW2.TestHW2 (testHW2) where

import Test.Hspec (SpecWith, describe)

import HW2.UnitTest1.TestNonEmpty (testsNonEmpty)
import HW2.UnitTest1.TestStringSum (testsStringSum)
import HW2.UnitTest1.TestTree (testsTree)
import HW2.UnitTest2.TestEvalExpr (testsEvalExpr)
import HW2.UnitTest2.TestSimp (testsSimplAverage)
import HW2.UnitTest3.TestCopyPaste (testsCopyPaste)

testHW2 :: SpecWith ()
testHW2 =
  describe "Tests for HomeWork 2" $ do
    testsStringSum
    testsTree
    testsNonEmpty
    testsEvalExpr
    testsSimplAverage
    testsCopyPaste
