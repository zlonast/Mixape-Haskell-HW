module HW1.TestHW1 (testHW1) where

import Test.Hspec (SpecWith, describe)

import HW1.UnitTest1.TestDay (testsWeeksDay)
import HW1.UnitTest1.TestMyNat (testsMyNat)
import HW1.UnitTest1.TestTree (testsTree)
import HW1.UnitTest2.TestFoldTree (testsFoldTree)
import HW1.UnitTest2.TestSplit (testsSplit)
import HW1.UnitTest3.TestConcat (testsMaybe)
import HW1.UnitTest3.TestSemigroup (testsSemigroup)

testHW1 :: SpecWith ()
testHW1 =
  describe "Tests for HomeWork 1" $ do
    testsWeeksDay
    testsMyNat
    testsTree
    testsFoldTree
    testsSplit
    testsMaybe
    testsSemigroup
