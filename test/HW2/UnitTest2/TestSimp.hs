{-# LANGUAGE TypeApplications   #-}
module HW2.UnitTest2.TestSimp where

import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW2.Unit2.MakeAverageGreatAgain ( moving )

testsSimplAverage :: SpecWith ()
testsSimplAverage =
  describe "Task5 - very simpl average" $ do
    testSimpl

testSimpl :: SpecWith ()
testSimpl = it "testSimpl" $ do
  moving @Double 4 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
  moving @Double 2 [1, 5, 3, 8, 7, 9, 6] `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
























