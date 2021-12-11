module HW2.UnitTest1.TestStringSum (testsStringSum) where


import Test.Hspec (SpecWith, describe, it, shouldBe)

import HW2.Unit1.StringSum (stringSum)

testsStringSum :: SpecWith ()
testsStringSum =
  describe "Task1 - String to Sum Int if is valid or Nothing" $ do
    testSimpl

testSimpl :: SpecWith ()
testSimpl = it "testSimpl" $ do
  stringSum "1 2 3 4" `shouldBe` Just 10
  stringSum "4  4  4  4" `shouldBe` Just 16
  stringSum "1 2  3  5" `shouldBe` Just 11
  stringSum "1 2  3  x4" `shouldBe` Nothing 
  stringSum "200" `shouldBe` Just 200
  stringSum "5x00 3x00" `shouldBe` Nothing























